module lending_protocol::lending {
    // std imports
    use std::option;
    use std::signer;
    use std::string::{Self, String};
    use std::vector;

    // aptos imports
    use aptos_framework::account;
    use aptos_std::math64;
    use aptos_std::simple_map::{Self, SimpleMap};
    use aptos_framework::fungible_asset::{
        Self,
        FungibleAsset,
        Metadata,
        MintRef,
        BurnRef,
        TransferRef,
        FungibleStore
    };
    use aptos_framework::object::{Self, Object, ExtendRef};
    use aptos_framework::primary_fungible_store;

    // constants
    const KTOKEN_DECIMALS: u8 = 8;
    const KTOKEN_NAME_PREFIX: vector<u8> = b"KToken ";
    const KTOKEN_SYMBOL_PREFIX: vector<u8> = b"K";
    const BPS_BASE: u64 = 10000;
    const DEFAULT_LIQUIDATION_INCENTIVE_BPS: u64 = 50;
    const DEFAULT_CLOSE_FACTOR_BPS: u64 = 2000;

    // errors
    const ERR_UNAUTHORIZED: u64 = 1;
    const ERR_SHORTFALL: u64 = 2;
    const ERR_NO_SHORTFALL: u64 = 3;
    const ERR_INSUFFICIENT_BALANCE: u64 = 4;
    const ERR_MARKET_MISMATCH: u64 = 5;
    const ERR_REPAY_OVER: u64 = 6;
    const ERR_LIQUIDATE_SELF: u64 = 7;

    struct Lending has key {
        liquidation_incentive_bps: u64,
        close_factor_bps: u64
    }

    struct Market has key {
        extend_ref: ExtendRef,
        ktoken_mint_ref: MintRef,
        ktoken_burn_ref: BurnRef,
        ktoken_transfer_ref: TransferRef,
        total_borrow: u64
    }

    struct Vault has key {
        collaterals: vector<Object<Market>>,
        debts: SimpleMap<Object<Market>, u64>
    }

    fun init_module(admin: &signer) {
        // assert!(signer::address_of(admin) == @lending_protocol, ERR_UNAUTHORIZED);
        move_to(
            admin,
            Lending {
                liquidation_incentive_bps: DEFAULT_LIQUIDATION_INCENTIVE_BPS,
                close_factor_bps: DEFAULT_CLOSE_FACTOR_BPS
            }
        );
    }

    public fun init_market(
        admin: &signer, underlying_asset: Object<Metadata>
    ): Object<Market> {
        assert!(signer::address_of(admin) == @lending_protocol, ERR_UNAUTHORIZED);

        let ktoken_name =
            ktoken_name_via_asset_name(fungible_asset::name(underlying_asset));
        let constructor_ref =
            object::create_named_object(admin, *string::bytes(&ktoken_name));
        let market_signer = object::generate_signer(&constructor_ref);

        // Underlying asset fungible store
        fungible_asset::create_store(&constructor_ref, underlying_asset);

        // Initialize KTOKEN
        let (ktoken_mint_ref, ktoken_burn_ref, ktoken_transfer_ref) = {
            let ktoken_symbol =
                ktoken_symbol_via_asset_symbol(fungible_asset::symbol(underlying_asset));
            let constructor_ref =
                object::create_named_object(&market_signer, *string::bytes(&ktoken_name));
            primary_fungible_store::create_primary_store_enabled_fungible_asset(
                &constructor_ref,
                option::none(),
                ktoken_name,
                ktoken_symbol,
                KTOKEN_DECIMALS,
                string::utf8(b"http://example.com/favicon.ico"),
                string::utf8(b"http://example.com")
            );
            (
                fungible_asset::generate_mint_ref(&constructor_ref),
                fungible_asset::generate_burn_ref(&constructor_ref),
                fungible_asset::generate_transfer_ref(&constructor_ref)
            )
        };

        move_to(
            &market_signer,
            Market {
                extend_ref: object::generate_extend_ref(&constructor_ref),
                ktoken_mint_ref,
                ktoken_burn_ref,
                ktoken_transfer_ref,
                total_borrow: 0
            }
        );

        object::object_from_constructor_ref<Market>(&constructor_ref)
    }

    public fun supply(
        account: &signer, market_obj: Object<Market>, underlying_fa: FungibleAsset
    ): FungibleAsset acquires Vault, Market {
        assert!(
            fungible_asset::asset_metadata(&underlying_fa)
                == fungible_asset::store_metadata(market_obj),
            ERR_MARKET_MISMATCH
        );

        let underlying_amount = fungible_asset::amount(&underlying_fa);

        // update market fungible store
        fungible_asset::deposit(market_obj, underlying_fa);

        // mint ktoken
        let ktoken_amount = underlying_to_ktoken(market_obj, underlying_amount);
        let market = borrow_global_mut<Market>(object::object_address(&market_obj));
        let ktoken = fungible_asset::mint(&market.ktoken_mint_ref, ktoken_amount);

        // update user vault
        init_vault_if_not_exists(account);
        let vault = borrow_global_mut<Vault>(signer::address_of(account));
        if (!vector::contains(&vault.collaterals, &market_obj)) {
            vector::push_back(&mut vault.collaterals, market_obj);
        };

        ktoken
    }

    public fun redeem(
        redeemer: address, market_obj: Object<Market>, ktoken_fa: FungibleAsset
    ): FungibleAsset acquires Market, Vault {
        assert!(
            ktoken_metadata(market_obj) == fungible_asset::asset_metadata(&ktoken_fa),
            ERR_MARKET_MISMATCH
        );

        // burn ktoken
        let ktoken_amount = fungible_asset::amount(&ktoken_fa);
        let underlying_amount = ktoken_to_underlying(market_obj, ktoken_amount);
        let market = borrow_global<Market>(object::object_address(&market_obj));
        fungible_asset::burn(&market.ktoken_burn_ref, ktoken_fa);

        // update market fungible store
        let market_signer = object::generate_signer_for_extending(&market.extend_ref);
        let underlying =
            fungible_asset::withdraw(&market_signer, market_obj, underlying_amount);

        let (_, shortfall) = account_liquidity(redeemer);
        assert!(shortfall == 0, ERR_SHORTFALL);

        underlying
    }

    public fun borrow(
        borrower: address, market_obj: Object<Market>, amount: u64
    ): FungibleAsset acquires Market, Vault {
        // update market fungible store and total_borrow
        let market = borrow_global_mut<Market>(object::object_address(&market_obj));
        let total_borrow = &mut market.total_borrow;
        *total_borrow = *total_borrow + amount;
        let market_signer = object::generate_signer_for_extending(&market.extend_ref);
        let fa = fungible_asset::withdraw(&market_signer, market_obj, amount);

        // update user vault
        let vault = borrow_global_mut<Vault>(borrower);
        if (!simple_map::contains_key(&vault.debts, &market_obj)) {
            simple_map::add(&mut vault.debts, market_obj, amount);
        } else {
            let debt = simple_map::borrow_mut(&mut vault.debts, &market_obj);
            *debt = *debt + amount;
        };

        let (_, shortfall) = account_liquidity(borrower);
        assert!(shortfall == 0, ERR_SHORTFALL);

        fa
    }

    public fun repay(
        repayer: address, market_obj: Object<Market>, fa: FungibleAsset
    ) acquires Vault, Market {
        assert!(
            fungible_asset::asset_metadata(&fa)
                == fungible_asset::store_metadata(market_obj),
            ERR_MARKET_MISMATCH
        );

        let amount = fungible_asset::amount(&fa);

        // update market fungible store
        fungible_asset::deposit(market_obj, fa);
        let market = borrow_global_mut<Market>(object::object_address(&market_obj));
        let total_borrow = &mut market.total_borrow;
        *total_borrow = *total_borrow - amount;

        // update user vault
        let vault = borrow_global_mut<Vault>(repayer);
        let debt = simple_map::borrow_mut(&mut vault.debts, &market_obj);
        *debt = *debt - amount;
    }

    public fun liquidate(
        collateral_market: Object<Market>,
        borrow_market: Object<Market>,
        liquidator: address,
        borrower: address,
        repay_fa: FungibleAsset
    ): FungibleAsset acquires Vault, Market, Lending {
        assert!(
            fungible_asset::asset_metadata(&repay_fa)
                == fungible_asset::store_metadata(borrow_market),
            ERR_MARKET_MISMATCH
        );
        assert!(borrower != liquidator, ERR_LIQUIDATE_SELF);

        let (_, shortfall) = account_liquidity(borrower);
        assert!(shortfall > 0, ERR_NO_SHORTFALL);

        let lending = borrow_global<Lending>(@lending_protocol);
        let repay_amount = fungible_asset::amount(&repay_fa);
        let borrow_amount = borrow_amount(borrow_market, borrower);
        let max_close_amount =
            math64::mul_div(borrow_amount, lending.close_factor_bps, BPS_BASE);
        assert!(repay_amount <= max_close_amount, ERR_REPAY_OVER);

        repay(liquidator, borrow_market, repay_fa);

        // transfer borrower ktoken to liquidator
        // seizeAmount = repayAmount * liquidationIncentive * priceBorrowed / priceCollateral
        // seizeTokens = seizeAmount / exchangeRate
        // = repayAmount * (liquidationIncentive * priceBorrowed) / (priceCollateral * exchangeRate)
        let seize_ktoken_amount = {
            let price_borrowed =
                asset_price(fungible_asset::store_metadata(borrow_market));
            let price_collateral =
                asset_price(fungible_asset::store_metadata(collateral_market));
            let b =
                math64::mul_div(
                    lending.liquidation_incentive_bps, price_borrowed, BPS_BASE
                );
            let (numerator, denominator) = exchange_rate(collateral_market);
            let c = math64::mul_div(price_collateral, numerator, denominator);
            math64::mul_div(repay_amount, b, c)
        };
        assert!(
            seize_ktoken_amount <= ktoken_balance(collateral_market, borrower),
            ERR_INSUFFICIENT_BALANCE
        );

        let ktoken_transfer_ref =
            &borrow_global<Market>(object::object_address(&collateral_market)).ktoken_transfer_ref;
        fungible_asset::withdraw_with_ref(
            ktoken_transfer_ref,
            primary_fungible_store::ensure_primary_store_exists(
                borrower,
                fungible_asset::transfer_ref_metadata(ktoken_transfer_ref)
            ),
            seize_ktoken_amount
        )
    }

    /// Returns (liquidity, shortfall)
    public fun account_liquidity(owner: address): (u64, u64) acquires Vault, Market {
        let vault = borrow_global<Vault>(owner);

        let collateral_value = vector::fold(
            vault.collaterals,
            0u64,
            |acc, market_obj| {
                let ktoken_balance = ktoken_balance(market_obj, owner);
                let underlying_balance = ktoken_to_underlying(market_obj, ktoken_balance);
                let underlying_metadata = fungible_asset::store_metadata(market_obj);
                let price = asset_price(underlying_metadata);
                acc
                    + math64::mul_div(
                        underlying_balance,
                        price,
                        math64::pow(
                            10, (fungible_asset::decimals(underlying_metadata) as u64)
                        )
                    )
            }
        );

        let liability_value = {
            let (market_objs, debts) = simple_map::to_vec_pair(vault.debts);
            let sum = 0;
            let i = 0;
            let n = vector::length(&market_objs);
            while (i < n) {
                let market_obj = *vector::borrow(&market_objs, i);
                let underlying_metadata = fungible_asset::store_metadata(market_obj);
                let underlying_balance = *vector::borrow(&debts, i);
                let price = asset_price(underlying_metadata);
                sum
                    + math64::mul_div(
                        underlying_balance,
                        price,
                        math64::pow(
                            10, (fungible_asset::decimals(underlying_metadata) as u64)
                        )
                    );
                i = i + 1;
            };
            sum
        };

        if (collateral_value > liability_value) {
            (collateral_value - liability_value, 0)
        } else {
            (0, liability_value - collateral_value)
        }
    }

    // getter/view functions

    #[view]
    /// ktoken_amount = amount / exchange_rate = amount * denominator / numerator
    public fun underlying_to_ktoken(
        market_obj: Object<Market>, underlying_amount: u64
    ): u64 acquires Market {
        let (numerator, denominator) = exchange_rate(market_obj);
        math64::mul_div(underlying_amount, denominator, numerator)
    }

    #[view]
    /// amount = ktoken_amount * exchange_rate = ktoken_amount * numerator / denominator
    public fun ktoken_to_underlying(
        market_obj: Object<Market>, ktoken_amount: u64
    ): u64 acquires Market {
        let (numerator, denominator) = exchange_rate(market_obj);
        math64::mul_div(ktoken_amount, numerator, denominator)
    }

    #[view]
    /// Return exchange rate between asset and ktoken
    /// TODO: count total_reserve
    public fun exchange_rate(market_obj: Object<Market>): (u64, u64) acquires Market {
        let cash = fungible_asset::balance(market_obj);

        let market = borrow_global<Market>(object::object_address(&market_obj));
        let total_borrow = market.total_borrow;

        // TODO: avoid the cast
        let total_supply = (ktoken_supply(market_obj) as u64);

        (cash + total_borrow, total_supply)
    }

    #[view]
    // TODO: IMPLEMENT ME
    public fun asset_price(_asset: Object<Metadata>): u64 {
        1
    }

    #[view]
    public fun borrow_amount(
        market_obj: Object<Market>, borrower: address
    ): u64 acquires Vault {
        let vault = borrow_global<Vault>(borrower);
        let debt = simple_map::borrow(&vault.debts, &market_obj);
        *debt
    }

    inline fun get_market_signer(market_obj: Object<Market>): signer acquires Market {
        let ref = &borrow_global<Market>(object::object_address(&market_obj)).extend_ref;
        object::generate_signer_for_extending(ref)
    }

    fun init_vault_if_not_exists(account: &signer) {
        if (!exists<Vault>(signer::address_of(account))) {
            let vault = Vault {
                collaterals: vector::empty(),
                debts: simple_map::create()
            };
            move_to(account, vault);
        };
    }

    // utilities

    fun ktoken_supply(market_obj: Object<Market>): u128 acquires Market {
        let asset = ktoken_metadata(market_obj);
        option::destroy_some(fungible_asset::supply(asset))
    }

    fun ktoken_balance(market_obj: Object<Market>, owner: address): u64 acquires Market {
        let store = ktoken_store(market_obj, owner);
        fungible_asset::balance(store)
    }

    fun ktoken_metadata(market_obj: Object<Market>): Object<Metadata> acquires Market {
        let market = borrow_global<Market>(object::object_address(&market_obj));
        fungible_asset::mint_ref_metadata(&market.ktoken_mint_ref)
    }

    fun ktoken_store(
        market_obj: Object<Market>, owner: address
    ): Object<FungibleStore> acquires Market {
        let asset = ktoken_metadata(market_obj);
        primary_fungible_store::ensure_primary_store_exists(owner, asset)
    }

    fun ktoken_name_via_asset_name(coin_name: String): String {
        let s = &mut string::utf8(KTOKEN_NAME_PREFIX);
        string::append(s, coin_name);
        *s
    }

    fun ktoken_symbol_via_asset_symbol(coin_symbol: String): String {
        let s = &mut string::utf8(KTOKEN_SYMBOL_PREFIX);
        string::append(s, coin_symbol);
        *s
    }

    #[test(admin = @0x3)]
    // #[test(admin = @lending_protocol)]
    public fun test_init_module(admin: &signer) acquires Lending {
        // create amount
        // account::create_account_for_test(signer::address_of(admin));
        // call initial_market_script
        init_module(admin);
        let lending = borrow_global<Lending>(signer::address_of(admin));
        // assert!(object::is_owner(lending,signer::address_of(admin)),6);

        assert!(lending.liquidation_incentive_bps == DEFAULT_LIQUIDATION_INCENTIVE_BPS, 3)
        // test market data
        // let market_addr = signer::address_of(market);
        // let market_signer_address = get_market_signer_address(market_addr);
        // let market_data = borrow_global_mut<MarketData>(market_signer_address);

        // assert!(market_data.fund_address == market_addr, 0);

    }
}
