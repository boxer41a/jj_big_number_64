note
	description: "[
		Adds features to {GMP_INTEGER} that are not in the original
		class but are useful for {BIG_NATURAL_TESTS}."
		]"
	author: "Jimmy J. Johnson"
	date: "5/21/20"

class
	JJ_GMP_INTEGER

inherit

	GMP_INTEGER
		rename
			quotient as float_quotient,
			integer_remainder as mod
		end

create
	default_create,
	make_by_pointer,
	make_integer_32,
	make_gmp_float,
	make_gmp_rational,
	make_natural_32,
	make_real_64,
	make_string

feature -- Status report

	is_negative: BOOLEAN
			-- Is Current less than zero?
		do
			Result := {MPZ_FUNCTIONS}.mpz_sgn (Current.item) < 0
		end

feature -- Query

	is_magnitude_equal (a_other: like Current): BOOLEAN
			-- Is the absolute value of `Current' equal to the
			-- absolute value of `a_other'?
		do
			Result := {MPZ_FUNCTIONS}.mpz_cmpabs (item, a_other.item) = 0
		end

	is_magnitude_less (a_other: like Current): BOOLEAN
			-- Is the absolute value of `Current' less than the
			-- absolute value of `a_other'?
		do
			Result := {MPZ_FUNCTIONS}.mpz_cmpabs (item, a_other.item) < 0
		end

	is_same_sign (a_other: like Current): BOOLEAN
			-- Are Current and `a_other' both positive or both negative?
		do
			Result := {MPZ_FUNCTIONS}.mpz_sgn (Current.item) = {MPZ_FUNCTIONS}.mpz_sgn (a_other.item)
		end

feature -- Basic operations

	power alias "^" (a_power: NATURAL_32): like Current
			-- Current raised to `a_other' power
		do
			create Result
			{MPZ_FUNCTIONS}.mpz_pow_ui (Result.item, item, a_power)
		end

	power_modulo (a_power, a_modulo: like Current): like Current
			-- Current raised to `a_other' power
		require
			exponent_big_enough: a_power > zero
			divisor_not_zero: not a_modulo.is_even
		do
			create Result
			{MPZ_FUNCTIONS}.mpz_powm_sec (Result.item, item, a_power.item, a_modulo.item)
		end

	scalar_add (a_value: NATURAL_32): like Current
			-- Result of adding `a_value' to Current.
		do
			create Result
			{MPZ_FUNCTIONS}.mpz_add_ui (Result.item, item, a_value.item)
		end

	scalar_subtract (a_value: NATURAL_32): like Current
			-- Result of adding `a_value' to Current.
		do
			create Result
			{MPZ_FUNCTIONS}.mpz_sub_ui (Result.item, item, a_value.item)
		end

	quotient (a_divisor: like Current): TUPLE [quot, rem: like Current]
			-- The result as quotient and remainder of dividing
			-- Currentby `a_divisor'.
			-- This uses the "truncate" division style which "rounds
			-- the [`quot'] towards zero, and [`rem'] will have the
			-- same sign as [Current]."
		local
			quot, rem: like Current
		do
			create quot
			create rem
			Result := [quot, rem]
			{MPZ_FUNCTIONS}.mpz_tdiv_qr (Result.quot.item, Result.rem.item, item, a_divisor.item)
		end

	integer_remainder alias "\\" (a_divisor: like Current): like Current
			-- The remainder returned by `quotient'.
		local
			quot, rem: like Current
			tup: TUPLE [quot, rem: like Current]
		do
			create quot
			create rem
			tup := [quot, rem]
			{MPZ_FUNCTIONS}.mpz_tdiv_qr (quot.item, rem.item, item, a_divisor.item)
			Result := tup.rem
		end

end
