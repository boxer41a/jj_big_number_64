note
	description: "[
		An object of this type represents a numbers that can be much larger than 
		what is representable in 32 or 64 bits.

		This class implements a big number as a list of bigits, where each `bigit'
		(rhymes with digit) represents one	"digit" (sometimes call a "limb" in
		other big-number implementaions).  For example, a  big-number class
		implemented using an 8-bit values for each bigit stores the base-10
		number "67,305,985" as a list of bigits <<4,3,2,1>>  which equals
		4*256^3 + 3*256^2 + 2*256^1 + 1*256^0.

		The "base" for this 64-bit implementation is 18,446,744,073,709,551,616,
		{NATURAL_64_REF}.max_value + 1.

		Feature `bigit' is {ARRAYED_LIST}.item renamed, and it is NOT exported; use `i_th'
		to obtain a particular bigit.

		Bigits are stored low-order to high-order.

		Manu said [SIC] in a newsgroup message at
			https://groups.google.com/forum/#!topic/eiffel-users/gdas7fprg7Q
-- Is this right?
		that the `count' of all Eiffel containers has a theoretical maximum of
		{INTEGER_32}.max_value which is 2^(32-1) or 2,147,483,647.  The practicle
		maximum, though is limited by a maximum object size, which is about 2^27 - 1
		or 134,217,727 bytes.  Dividing this limitting number by, 8 bytes (for a
		64-bit representation) implies that a {JJ_BIG_NATURAL} can contain over
		16.5 million bigits, which could represent a number over:
		     (18.4*10^12)^(16.5*10^6) > 10^(10^38).

		In theory, implementing Current as a list of arrays (instead of just one
		array) and changing the `count' and related attributes to like Current could
		make the number of bigits and therefore the magnitude of the number limited
		only by the amount of memory.

		I need to add preconditions to the math functions (and any feature that
		extends bigits) to prevent adding more bigits than Current can handle.
-- end is this right?
	]"
	author: "Jimmy J.Johnson"

class
	JJ_BIG_NUMBER_64

inherit

	NUMERIC
		rename
			quotient as integer_quotient alias "//"
		undefine
			copy,
			is_equal,
			default_create,
			out
		end

	COMPARABLE
		undefine
			copy
		redefine
			default_create,
			out
		end

	HASHABLE
		undefine
			copy,
			is_equal
		redefine
			default_create,
			out
		end

	SINGLE_MATH			-- for `log_2' in division algorithms
		undefine
			copy,
			is_equal
		redefine
			default_create,
			out
		end

	ARRAYED_LIST [NATURAL_64]
		rename
			item as bigit		-- rhymes with digit (i.e. a big digit)
		export
			{NONE}
				all
			{JJ_BIG_NUMBER_64}
				prunable,
				extend,
-- pick one of the next three feature, no need to export/use all
-- they seem to do the same thing in this context.
				insert,
				put_i_th,
				put_front,
				extendible,
				area_v2,
				before,
				after,
				off,
				is_empty,
				index
			{ANY}
				i_th,
				valid_index,
				count
		undefine
			is_equal
		redefine
			default_create,
			out,
			new_filled_list
		end

create
	default_create,
	from_integer,
	from_value,
	from_array,
	from_string,
	make_random,
	make_random_with_digit_count

create {ARRAYED_LIST}
	make

convert
	from_string ({STRING_8})

feature {NONE} -- Initialization

	default_create
			-- Initialize current
		do
			make (10)
			extend (zero_bigit)
		end

	from_integer (a_integer: INTEGER)
			-- Initialize Current with `a_integer'.
		do
			default_create
			set_with_integer (a_integer)
		end

	from_value (a_value: NATURAL_64)
			-- Initialize Current with `a_value'
		local
			r, c: NATURAL_64
		do
			default_create
			set_with_value (a_value)
		end

	from_string (a_string: STRING_8)
			-- Create an instance from `a_value'.
		require
			string_long_enough: a_string.count >= 1
			is_number: is_valid_string (a_string)
		do
			default_create
			set_with_string (a_string)
		end

	from_array (a_array: ARRAY [NATURAL_64])
			-- Create an instance from `a_array', where the array holds the
			-- intended bigits with high-order bigits first.
		require
			array_exists: a_array /= Void
			array_not_empty: not a_array.is_empty
			has_valid_bigits: across a_array as it all it.item <= max_bigit end
		local
			i: INTEGER
		do
			default_create
			set_with_array (a_array)
		end

	make_random (a_count: INTEGER)
			-- Create a random number containing `a_count' bigits.
		require
			count_big_enough: a_count >= 1
		do
			default_create
			set_random (a_count)
		ensure
			correct_count: count = a_count
		end

	make_random_with_digit_count (a_count: INTEGER)
			-- Create a random number ccontaining `a_count' decimal digits.
		do
			default_create
			set_random_with_digit_count (a_count)
		ensure
			correct_count: out.count = a_count
		end

feature -- Initialization

	set_with_integer (a_integer: INTEGER)
			-- Make Current equivalent to `a_integer'.
		local
			b, x, r: NATURAL_64
		do
			check
				not_to_big: a_integer.abs.to_natural_32 <= max_bigit
					-- Because that is biggest integer magnitude possible
			end
			wipe_out
			set_with_value (a_integer.abs.as_natural_32)
			if a_integer < 0 then
				set_is_negative (true)
			end
		ensure
			correct_initialization: Current.out ~ a_integer.out
			correct_negative: a_integer < 0 implies Current.is_negative
		end

	set_with_value (a_value: NATURAL_64)
			-- Make Current equivalent to `a_value'.
		local
			r, c: NATURAL_64
			v: NATURAL_64
		do
			wipe_out
			set_is_negative (false)
			extend (a_value)
		ensure
			has_one_bigit: count = 1
		end

	is_valid_string (a_string: STRING_8): BOOLEAN
			-- Is `a_string' in the correct format?
			-- Yes if it contains only characters '0'..'9', optional
			-- commas, and optional leading '+' or '-'.
			-- (The placement of commas is unimportant and ignored.)
			-- Valid strings:
			--    "-99,505,999"
			--    "9950,599,9"
			--    ""
		require
			string_exists: a_string /= Void
		local
			s, ss: STRING_8
			i: INTEGER
		do
			Result := true
			s := a_string.twin
			s.prune_all (',')
					-- Is the first character a plus or minus sign?
			ss := s.substring (1, 1)
			if ss ~ "+" or ss ~ "-" then
				s.keep_tail (s.count - 1)
			else
				do_nothing
			end
			from i := 1
			until i > s.count or not Result
			loop
				Result := s.item (i).is_digit
				i := i + 1
			end
		end

	set_with_string (a_string: STRING_8)
			-- Set Current as on a string given in base 10.
			-- If `a_string' is empty, then Current is equivalent to zero.
		require
			is_number: is_valid_string (a_string)
		local
			s, ss: STRING_8
			neg: BOOLEAN
			i: INTEGER
			place: like Current
			d: NATURAL_64

			temp: like Current
		do
			Default_create
			if a_string.count > 0 then
				s := a_string.twin
				s.prune_all (',')
					-- Is the first character a plus or minus sign?
				ss := s.substring (1, 1)
				if ss ~ "-" then
					neg := true
					s.keep_tail (s.count - 1)
				elseif s ~ "+" then
					s.keep_tail (s.count - 1)
				else
					do_nothing
				end
				place := get_number (one_bigit)
				from i := s.count
				until i < 1
				loop
						-- for each bigit, multiply by place and add
					d := new_value_from_character (s.item (i))
					temp := place.scalar_product (d)
					add (place.scalar_product (d))
					place.scalar_multiply (ten_bigit)
					i := i - 1
				end
					-- Done with `place'.
				put_number (place)
			end
			if neg then
				set_is_negative (true)
			end
		end

	set_with_array (a_array: ARRAY [NATURAL_64])
			-- Set the bigits from `a_array', where the array holds the
			-- intended bigits with high-order bigits first.
		require
			array_exists: a_array /= Void
			array_not_empty: not a_array.is_empty
		local
			i: INTEGER
			d: NATURAL_64
			place: like Current
		do
			wipe_out
			from i := a_array.count
			until i < 1
			loop
				d := a_array [i]
				extend (a_array [i])
				i := i - 1
			end
		end

	set_random (a_count: INTEGER)
			-- Set Current as a random number with `a_count' number
			-- of bigits (i.e. limbs).
		require
			count_big_enough: a_count >= 1
		local
			i: INTEGER
			r: NATURAL_64
			dn, up: NATURAL_64
		do
			dn := random.lower
			up := random.upper
			wipe_out
			from i := 1
			until i > a_count - 1
			loop
				random.forth
				r := random.item
				extend (r)
				i := i + 1
			end
				-- High order bigit must be greater than zero
			random.set_range (one_bigit, max_bigit)
			extend (random.item)
			random.set_range (dn, up)
		ensure
			correct_count: count = a_count
		end

	set_random_with_digit_count (a_count: INTEGER)
			-- Set Current as a random number with `a_count' number
			-- of decimal digits.
			-- Warning:  this feature is very slow.
		require
			count_big_enough: a_count >= 1
		local
			s: STRING_8
		do
				-- Simple, but slow?
				-- Create a string with random digits
			s := random_string (a_count)
			set_with_string (s)
		ensure
			correct_count: out.count = a_count
		end

feature -- Constants

--	lg_10: REAL_32 = 3.3219280949
--			-- Log base 2 of ten.

	bits_per_bigit: INTEGER_32 = 64
			-- The number of bits in each bigit of Current in same type as `bigit'.

	zero_bigit: NATURAL_64 = 0
			-- The number zero in the same type as `bigit'

	one_bigit: NATURAL_64 = 1
			-- The number one in the same type as `bigit'

	two_bigit: NATURAL_64 = 2
			-- The number two in the same type as `bigit'

	three_bigit: NATURAL_64 = 3
			-- The number two in the same type as `bigit'

	four_bigit: NATURAL_64 = 4
			-- The number two in the same type as `bigit'

	five_bigit: NATURAL_64 = 5
			-- The number two in the same type as `bigit'

	six_bigit: NATURAL_64 = 6
			-- The number two in the same type as `bigit'

	seven_bigit: NATURAL_64 = 7
			-- The number two in the same type as `bigit'

	eight_bigit: NATURAL_64 = 8
			-- The number two in the same type as `bigit'

	nine_bigit: NATURAL_64 = 9
			-- The number ten in the same type as `bigit'

	ten_bigit: NATURAL_64 = 10
			-- The number two in the same type as `bigit'

	sixteen_bigit: NATURAL_64 = 16
			-- The number 16 in the same type as `bigit'.

	max_ten_power: NATURAL_64 = 10_000_000_000_000_000_000
			-- Largest multiple of 10 representable in a bigit of Current.

	max_half_bigit: NATURAL_64 = 0x00000000FFFFFFFF
			-- The largest value representable in half the number of
			-- bits in Current's representation of a `bigit'.

	max_bigit: NATURAL_64 = 0xFFFFFFFFFFFFFFFF
			-- The largest value allowed for a `bigit' of Current (i.e. all ones).

	ones (a_count: INTEGER): like Current
			-- A big number containing `a_count' bigits where each bigit
			-- is all ones (i.e. each bigit contains the `max_bigit').
		require
			count_big_enough: a_count >= 1
		local
			i: INTEGER
		do
			Result := new_big_number (max_bigit)
			from i := 2
			until i > a_count
			loop
				Result.extend (max_bigit)
				i := i + 1
			end
		end

	zeros (a_count: INTEGER): like Current
			-- A big number containing `a_count' bigits where each bigit
			-- is all zeros (i.e. each bigit contains the `zero_bigit').
		require
			count_big_enough: a_count >= 1
		local
			i: INTEGER
		do
			Result := new_big_number (zero_bigit)
			from i := 2
			until i > a_count
			loop
				Result.extend (zero_bigit)
				i := i + 1
			end
		end

	Default_karatsuba_threshold: INTEGER = 4
			-- Default value for `karatsuba_threshold'.

	Default_div_limit: INTEGER = 4
			-- Default value for `div_limit'.

feature -- Access

	zero: like Current
			-- Neutral element for "+" and "-".
		do
			create Result
		end

	one: like Current
			-- Neutral element for "*" and "/".
		do
			create Result
			Result.put_i_th (one_bigit, 1)
		end

	frozen karatsuba_threshold: INTEGER
			-- The value above which multiplications use `karatsuba_product'.
			-- Change with `set_karatsuba_threshold'.
		do
			Result := karatsuba_threshold_imp.item
		ensure
			result_big_enough: Result >= 2
		end

	frozen div_limit: INTEGER
			-- Used by Burnikel and Zeigler division algorithm `two_by_one_divide'.
			-- The number of bigits in a divisor above which the B&Z algorithms
			-- recurse; below this number, division reverts to `school_division'.
		do
			Result := div_limit_imp.item
		ensure
			result_big_enough: Result >= 4
		end

	frozen hash_code: INTEGER
			-- Hash value computed from the string representation of Current.
		do
			Result := out_as_stored.hash_code
		end

	bit_count: INTEGER
			-- The number of binary digits (i.e. BITs) in Current.
		local
			i: INTEGER
		do
			Result := count * bits_per_bigit
		ensure
			definition: Result = count * bits_per_bigit
		end

	occupied_bit_count:  INTEGER
			-- The number of bits utilized to reprsent Current
			-- (i.e. count only up to the highest order bit set)
		local
			n: NATURAL_64
			c: INTEGER
		do
				-- Find the index of the most significant bit in the high-order bigit
			from n := i_th (count)
			until n <= 0
			loop
				c := c + 1
				n := n.bit_shift_right (1)
			end
			Result := (count - 1) * bits_per_bigit + c
		end

	decimal_count: INTEGER
			-- The [maximum] number of decimal digits in Current.
			-- May be too large by one.
			-- http://www.exploringbinary.com/number-of-decimal-digits-in-a-binary-integer.
			--     d_min = floor (log (2^(b-1) + 1) = floor ((b-1)*log(2) + 1)
			--     d_max = floor (log (2^b + 1) = floor (b*log(2) + 1)
		local
			lg2: like Current
			bc: like Current
			tens: like Current
		do
			print ("{JJ_BIG_NUMBER_64}.decimal_count:   Fix me %N")
--			Result := (occupied_bit_count * log(2) + 1).truncated_to_integer
		end

feature -- Element change

	set_karatsuba_threshold (a_value: INTEGER)
			-- Change the `karatsub_threshold', the number of bigits at which
			-- *all* multiplications of a {BIG_NUMBER} use the Karatsuba
			-- algorithm instead of a basic, grade-school method.
		require
			value_big_enough: a_value >= 1
		do
			karatsuba_threshold_imp.set_item (a_value)
		end

	set_div_limit (a_value: INTEGER)
			-- Change the `div_limit', the number of bigits in the divisor
			-- above which the B&Z algorithms recurse; below this number,
			-- division reverts to `school_division'.
		require
			value_big_enough: a_value >= 2
		do
			div_limit_imp.set_item (a_value)
		ensure
			value_was_set: div_limit = a_value
		end

feature -- Status setting

	set_is_negative (a_sign: BOOLEAN)
			-- Set `is_negative' to `a_sign' if Current /~ `zero'.
		do
			if is_zero then
				is_negative := false
			else
				is_negative := a_sign
			end
		ensure
			zero_is_positive: is_zero implies not is_negative
			positive_assigned: not a_sign implies not is_negative
			negative_assigned: not is_zero implies (a_sign implies is_negative)
		end

feature -- Status report

	is_zero: BOOLEAN
			-- Is Current equivalent to `zero'?
		local
			i: INTEGER
		do
			Result := true
				-- Some calculations can produce an intermediate {BIG_NUMBER}
				-- with more than one zero, so check all values.
			from i := 1
			until i > count or else not Result
			loop
				Result := i_th (i) = 0
				i := i + 1
			end
		end

	is_one: BOOLEAN
			-- Is Current's magnitude equivalent to `one'?
			-- Is Current equal to one or negative one?
		local
			i: INTEGER
		do
				-- Must account for leading zeros
			from i := count
			until i = 1 or else i_th (i) /= zero_bigit
			loop
				i := i - 1
			end
			Result := i = 1 and then i_th (1) = one_bigit and not is_negative
		end

	is_even: BOOLEAN
			-- Is Current an even number?
		do
			Result := i_th (1) \\ two_bigit = zero_bigit
		end

	is_base: BOOLEAN
			-- Is Current equal to the value of the [implementation]
			-- of the base?
		local
			i: INTEGER
		do
			if count >= 2 then
					-- Must account for leading zeros
				from i := count
				until i = 2 or else i_th (i) /= zero_bigit
				loop
					i := i - 1
				end
				Result := i = 2 and then (i_th (1) = zero_bigit and i_th (2) = one_bigit)
			end
		end

--	is_base_multiple: BOOLEAN
--			-- Is Current a power of the [implementation]
--			-- of the base, such as <<1, 0, 0, 0, 0>>?
--		local
--			i: INTEGER
--		do
--			if count >= 2 then
--				Result := true
--					-- Must account for leading zeros
--				from i := count
--				until i = 1 or else i_th (i) /= zero_bigit
--				loop
--					i := i - 1
--				end
--					-- Is first non-zero bigit a one?
--				Result := i_th (i) = one_bigit
--					-- Check the remaining bigits for all zeros.
--				from i := i - 1
--				until not Result or else i < 1
--				loop
--					Result := i_th (i) = zero_bigit
--				end
--			end
--		end

	is_negative: BOOLEAN
			-- Is Current a negative number?

	divisible (a_other: like Current): BOOLEAN
			-- May current object be divided by `a_other'?
		do
			Result := a_other /~ zero
		end

	exponentiable (a_other: NUMERIC): BOOLEAN
			-- May current object be elevated to the power `a_other'?
			-- Must be defined, because it comes deferred from {NUMERIC}.
		obsolete
			"[2008_04_01] Will be removed since not used."
		do
		end

feature -- Query

	is_same_sign (a_other: like Current): BOOLEAN
			-- Does Current have the same sign as `a_other'?
		do
			Result := is_negative = a_other.is_negative
		end

	is_less alias "<" (a_other: like Current): BOOLEAN
			-- Is Current less than `a_other'?
		do
			if a_other /= Current then
				if is_negative and not a_other.is_negative then
					Result := true
				elseif not is_negative and a_other.is_negative then
					Result := false
				elseif not is_negative and not a_other.is_negative then
					Result := is_magnitude_less (a_other)
				else
					Result := a_other.is_magnitude_less (Current)
				end
			end
		end

	is_magnitude_less (a_other: like Current): BOOLEAN
			-- Is Current's absolute value < `a_other's absolute value?
		require
			other_exists: a_other /= Void
		local
			c, oc: INTEGER
			i: INTEGER
			temp: like Current
		do
			if a_other /= Current then
					-- Find the index of the first non-zero bigit of Current.
				from i := count
				until i < 1 or else i_th (i) > zero_bigit
				loop
					i := i - 1
				end
				c := i
					-- Find the index of the first non-zero bigit of `a_other.
				from i := a_other.count
				until i < 1 or else a_other.i_th (i) > zero_bigit
				loop
					i := i - 1
				end
				oc := i
				if c < oc then
					Result := true
				elseif c > oc then
					Result := false
				else
						-- Both numbers have same non-zero bigit count.
					if c = 0 and oc = zero then
						Result := false
					else
						from i := c
						until i < 1 or i_th (i) > a_other.i_th (i) or Result
						loop
							Result := i_th (i) < a_other.i_th (i)
							i := i - 1
						end
					end
				end
			end
		end

	is_magnitude_equal (a_other: like Current): BOOLEAN
			-- Is the magnitude of Current the same as `a_other'?
			-- The signs may be different.
		local
			c, oc: INTEGER
			i: INTEGER
		do
			if Current = a_other then
				Result := true
			else
					-- Find the index of the first non-zero bigit of Current.
				from i := count
				until i < 1 or else i_th (i) > zero_bigit
				loop
					i := i - 1
				end
				c := i
					-- Find the index of the first non-zero bigit of `a_other.
				from i := a_other.count
				until i < 1 or else a_other.i_th (i) > zero_bigit
				loop
					i := i - 1
				end
				oc := i
				if c = oc then
					Result := true
					from i := c
					until not Result or i < 1
					loop
						Result := i_th (i) = a_other.i_th (i)
						i := i - 1
					end
				end
			end
		end

	magnitude_max (a_other: like Current): like Current
			-- The number with the largest absolute value.
		require
			other_exists: a_other /= Void
		local
			neg, o_neg: BOOLEAN
		do
			neg := is_negative
			o_neg := a_other.is_negative
				-- Set Current and a_other to positive
			set_is_negative (false)
			a_other.set_is_negative (false)
			if Current >= a_other then
				Result := Current
			else
				Result := a_other
			end
				-- Restore the sign of Current and a_other
			set_is_negative (neg)
			a_other.set_is_negative (o_neg)
		end

	magnitude_min (a_other: like Current): like Current
			-- The number with the smallest absolute value.
		require
			other_exists: a_other /= Void
		local
			neg: BOOLEAN
		do
			neg := a_other.is_negative
			a_other.set_is_negative (is_negative)
			if Current <= a_other then
				Result := Current
			else
				Result := a_other
			end
			a_other.set_is_negative (neg)
		end

feature -- Basic operations (simple)

	negate
			-- Reverse the sign of Current.
		do
			if not is_zero then
				is_negative := not is_negative
			end
		ensure
			sign_toggled: not is_zero implies is_negative = not (old is_negative)
		end

	increment
			-- Add one to Current.
		do
			add (one)
		end

	decrement
			-- Subtract one from Current.
		local
			n: like Current
		do
			n := one
			n.negate
			add (n)
		end

	identity alias "+": like Current
			-- Unary plus.
		do
			Result := twin
		end

	opposite alias "-" alias "−": like Current
--	opposite alias "-": like Current
			-- Unary minus.
		do
			Result := twin
			Result.negate
		end

	magnitude: like Current
			-- The absolute value of Current.
		do
			Result := twin
			Result.set_is_negative (false)
		ensure
			not_negative: not Result.is_negative
		end

feature -- Basic operations (addition & subtraction)

	scalar_add (a_value: NATURAL_64)
			-- Change Current by adding `a_value' to Current.
			-- By definition `a_value' will be no larger than the value
			-- representable by the type of a `bigit'.
		local
			n: like Current
		do
			n := new_big_number (a_value)
			add (n)
		end

	scalar_subtract (a_value: NATURAL_64)
			-- Change Current by subtracting `a_value' from Current.
			-- By definition `a_value' will be no larger than the value
			-- representable by the type of a `bigit'.
		local
			n: like Current
		do
			n := get_number (a_value)
			n.negate
			add (n)
			put_number (n)
		end

	scalar_sum (a_value: NATURAL_64): like Current
			-- The result of adding `a_bigit' to Current.
			-- Do not change Current.
		do
			Result := deep_twin
			Result.scalar_add (a_value)
		end

	scalar_difference (a_value: NATURAL_64): like Current
			-- The result of subtracting `a_bigit' from Current.
			-- Do not change Current.
		do
			Result := deep_twin
			Result.scalar_subtract (a_value)
		end

	add (a_other: like Current)
			-- Change Current by adding a_other to Current.
		require
			other_exists: a_other /= Void
		do
			add_imp (a_other)
			remove_leading_zeros
		end

	plus alias "+" (a_other: like Current): like Current
			-- New object containing the sum of Current and `a_other'.
		do
			Result := twin
			Result.add (a_other)
		end

	subtract (a_other: like Current)
			-- Subtract a_other from Current.
		require
			other_exists: a_other /= Void
		do
			subtract_imp (a_other)
			remove_leading_zeros
		end

	minus alias "-" alias "−" (a_other: like Current): like Current
--	minus alias "-" (a_other: like Current): like Current
			-- Result of subtracting `a_other' from Current.
			-- Does not change Current.
		do
			Result := twin
			Result.subtract (a_other)
		end

feature {JJ_BIG_NUMBER_64} -- Implementation (addition & subtraction)

	subtract_imp (a_other: like Current)
			-- Subtract a_other from Current.
			-- Implementation for `subtract' which keeps leading zeros.
		require
			other_exists: a_other /= Void
		do
			a_other.negate
			add_imp (a_other)
			a_other.negate
		end

	add_imp (a_other: like Current)
			-- Change Current by adding `a_other' to Current.
			-- Handles casses where Current and `a_other' have different signs.
			-- Implementation for `add' which keeps leading zeros.
		require
			other_exists: a_other /= Void
		local
			subtrahend, minuend: detachable like Current
			is_neg_o, sign_max: BOOLEAN
		do
				-- Save the sign a for later restoration.
			is_neg_o := a_other.is_negative
				-- Do the addition
			if is_same_sign (a_other) then
				simple_add (a_other)
			else
					-- minuend - subtrahend = difference
					-- Find the larger.
				minuend := magnitude_max (a_other)
				sign_max := minuend.is_negative
					-- Set both to positive.
				set_is_negative (false)
				a_other.set_is_negative (false)
					-- Find the subtrahend.
				if minuend = Current then
					subtrahend := a_other
				else
					subtrahend := Current
					minuend := minuend.twin
				end
				check
					same_signs: minuend.is_same_sign (subtrahend)
				end
				check
					sub_less: subtrahend.magnitude <= minuend.magnitude
				end
					-- Subtract smaller from larger.
				minuend.simple_subtract (subtrahend)
					-- Restore Current if it was not the larger.
				if minuend /= Current then
					copy (minuend)
				end
					-- Set the sign to that of larger.
				if not is_zero then
					set_is_negative (sign_max)
				end
					-- Restore sign of `a_other' in case it had changed.
				a_other.set_is_negative (is_neg_o)
			end
			if is_zero then
				is_negative := false
			end
		end

	simple_add (a_other: like Current)
			-- Change Current by adding `a_other' to it.
			-- Used internally for `add' and `subtract'.
		require
			other_exists: a_other /= Void
			same_sign: is_same_sign (a_other)
		local
			i: like count
			tup: TUPLE [sum, carry: NATURAL_64]
		do
--			if is_zero then
--					-- This drops zeroes when Current starts with more than one.
--				copy (a_other)
--			elseif not a_other.is_zero then
					-- Add each paired `bigit'.
				tup := [zero_bigit, zero_bigit]
				from i := 1
				until i > count or i > a_other.count
				loop
					bigits_added (i_th (i), a_other.i_th (i), tup.carry, tup)
					put_i_th (tup.sum, i)
					i := i + 1
				end
					-- Include the unpaired bigits.
				if i > count then
						-- Bring the rest of the bigits of a_other into Current.
					check
						finished_with_currents_bigits: i = count + 1
					end
					from
					until i > a_other.count
					loop
						bigits_added (zero_bigit, a_other.i_th (i), tup.carry, tup)
						extend (tup.sum)
						i := i + 1
					end
				elseif i > a_other.count then
						-- Add carry into Current's bigits.
					check
						finished_with_others_bigits: i = a_other.count + 1
					end
					from
					until i > count
					loop
						bigits_added (i_th (i), zero_bigit, tup.carry, tup)
						put_i_th (tup.sum, i)
						i := i + 1
					end
				end
					-- If still have a carry (overflow), add a bigit.
				if tup.carry > zero_bigit then
					extend (one_bigit)
				end
--			end
		end

	simple_subtract (a_other: like Current)
			-- Change Current by subtracting `a_other' from Current.
		require
			other_exists: a_other /= Void
			same_sign: is_same_sign (a_other)
			other_is_less: a_other.magnitude <= magnitude
		local
			i: INTEGER
		do
				-- Step through values changing Current (i.e. the minuend).
			from i := 1
			until i > count or i > a_other.count
			loop
				subtract_i_th (a_other.i_th (i), i)
				i := i + 1
			end
			check
				i >= a_other.count
					-- because the preconditions guarantee the subtrahend (a_other)
					-- to be less than the minuend (Current).
			end
			if is_zero then
				is_negative := false
			end
		end

	bigits_added (a, b, c_in: NATURAL_64; tup: TUPLE [sum, carry: NATURAL_64])
			-- Add two bigits and a carry bigit, giving a sum with a
			-- carry out, because simply adding the values might result
			-- in overflow with data lose.  The result is passed out in
			-- the `tup' in order to avoid creating a new TUPLE on every
			-- call to this feature.
			-- This decomposition feature is used by `simple_add'.
		require
			carry_in_one_or_zero: c_in <= one_bigit
		local
			d: NATURAL_64
			c: NATURAL_64
		do
			c := zero_bigit
			d := a + b
			if d < a or d < b then
				c := one_bigit
			end
			d := d + c_in
			if d < c_in then
				c := one_bigit
			end
			tup.sum := d
			tup.carry := c
		end

	can_borrow (a_index: INTEGER): BOOLEAN
			-- Is it possible to borrow from `a_index'th bigit?
			-- In a_other bigits, can we subtract one place-value amount from
			-- Current at `a_index' and add that amount to the lower-order
			-- bigit without overflowing the representation of a bigit?
		do
			Result := (a_index > 1 and a_index <= count)
		end

	borrow (a_index: INTEGER)
			-- Modify current by borrowing from the `a_index'-th bigit.
			-- The calling routine (i.e. the subtraction routine) must
			-- account for the value of the receiving bigit, because the
			-- borrowed amount (i.e the max base), which not representable
			-- cannot be added to that bigit.
		require
			index_large_enough: a_index > 1
			index_small_enough: a_index <= count
			can_borrow: can_borrow (a_index)
		local
			i: INTEGER
		do
			from i := a_index
			until i_th (i) > zero_bigit
			loop
				check
					eventually_find_a_borrow: i <= count
						-- because of precondition "can_borrow".
				end
				put_i_th (max_bigit, i)
				i := i + 1
			end
			put_i_th (i_th (i) - zero_bigit.one, i)
			if i = count and i_th (count) = 0 then
				go_i_th (count)
				remove
			end
		end

	subtract_i_th (a_value: NATURAL_64; a_index: INTEGER)
			-- Modify current by subtracting `a_value' from `a_index'th bigit,
			-- borrowing if necessary.
		require
			index_big_enough: a_index >= 1
			index_small_enough: a_index <= count
			subtraction_allowed: a_value <= i_th (a_index) or can_borrow (a_index + 1)
		local
			d: NATURAL_64
		do
			if i_th (a_index) < a_value then
				borrow (a_index + 1)
					-- Subtract first, then add the borrow;
				d := max_bigit - a_value + i_th (a_index) + one_bigit
				check
					positive_d: d > zero_bigit
						-- because, otherwise, would not have borrowed.
				end
			else
				d := i_th (a_index) - a_value
			end
			put_i_th (d, a_index)
		end

feature -- Basic operations (multiplication)

	scalar_multiply (a_value: NATURAL_64)
			-- Multipy Current by `a_value'.
		local
			c: INTEGER
			temp: like Current
			i: INTEGER
			prev_hi: NATURAL_64
			tup: TUPLE [high, low: NATURAL_64]
			t, t2: NATURAL_64
		do
			c := count.max (1)
			if is_zero then
				do_nothing
			elseif a_value = zero_bigit then
				wipe_out
				extend (zero_bigit)
				is_negative := false
			else
				temp := twin
				wipe_out
				tup := [zero_bigit, zero_bigit]
				prev_hi := zero_bigit
				from i := 1
				until i > temp.count
				loop
					bigits_multiplied (temp.i_th (i), a_value, tup)
					extend (tup.low + prev_hi)		-- throw out high-order bits
					if tup.low > max_bigit - prev_hi then
						prev_hi := tup.high + one_bigit	-- carry a one
					else
						prev_hi := tup.high
					end
					i := i + 1
				end
				if prev_hi > zero_bigit then
					extend (prev_hi)
				end
				if (is_negative and a_value >= zero_bigit) or
						(not is_negative and a_value < zero_bigit) then
					set_is_negative (true)
				else
					set_is_negative (false)
				end
			end
		ensure
--			correct_bigit_count: count = old count or count = old count + 1
		end

	scalar_product (a_value: NATURAL_64): like Current
			-- New object equivalent to Current multiplied by `a_value'.
			-- Do not change Current.
		require
			value_small_enough: a_value <= max_bigit
		do
			Result := twin
			Result.scalar_multiply (a_value)
		end

	multiply (a_other: like Current)
			-- Change Current by multiplying it by `a_other'.
		do
			copy (Current * a_other)
		end

	product alias "*" alias "×" (a_other: like Current): like Current
--	product alias "*" (a_other: like Current): like Current
			-- Product by `a_other'.
		do
				-- Unlike the relationship between `add' and `+', where the
				-- work is done in `add' and `+' is a copy; the work for
				-- multiplication is done in `*' and `multiply' performs the
				-- copy.  This results in fewer copy or twin operations.
			if is_zero or a_other.is_zero then
				Result := new_big_number (zero_bigit)
			elseif is_one then
				Result := a_other.deep_twin
			elseif a_other.is_one then
				Result := deep_twin
			elseif is_base then
				Result := a_other.deep_twin
				Result.shift_left (1)
			elseif a_other.is_base then
				Result := deep_twin
				Result.shift_left (1)
			else
				if count < karatsuba_threshold and a_other.count < karatsuba_threshold then
					Result := simple_product (a_other)
				elseif count >= karatsuba_threshold and a_other.count >= karatsuba_threshold then
					if (count - a_other.count).abs <= 1 then
						Result := karatsuba_product (a_other)
					else
						Result := recursive_product (a_other)
					end
				else
					Result := recursive_product (a_other)
				end
			end
			if is_same_sign (a_other) then
				Result.set_is_negative (false)
			else
				Result.set_is_negative (true)
			end
		ensure then
			zero_factor_implies_is_zero: (is_zero or a_other.is_zero) implies Result.is_zero
			zero_result_implies_count: Result.is_zero implies Result.count = 1
		end

feature {JJ_BIG_NUMBER_64} -- Implementation (multiplication)

	simple_product (a_other: like Current): like Current
			-- The result of multiplying Current and `a_other'.
			-- Grade-school style algorithim with complexity ~ O(n^2).
		require
			other_exists: a_other /= Void
			not_zero: not is_zero
			other_not_zero: not a_other.is_zero
			not_one: not is_one
			other_not_one: not a_other.is_one
			not_base: not is_base
			other_not_base: not a_other.is_base
			karatsuba_inapplicable: count < karatsuba_threshold and a_other.count < karatsuba_threshold
		local
			fac_1, fac_2: like Current
			s: like Current
			i: INTEGER
			d: NATURAL_64
		do
			if count < a_other.count then
				fac_1 := a_other
				fac_2 := Current
			else
				fac_1 := Current
				fac_2 := a_other
			end
			Result := new_big_number (zero_bigit)
			from i := 1
			until i > fac_2.count
			loop
				s := fac_1.twin
				d := fac_2.i_th (i)
				s.scalar_multiply (d)
				if not s.is_zero then
					s.shift_left (i - 1)
				end
				Result := Result + s
				i := i + 1
			end
			if is_same_sign (a_other) then
				Result.set_is_negative (false)
			else
				Result.set_is_negative (true)
			end
		end

	recursive_product (a_other: like Current): like Current
			-- Divide-and-conquer multiplication used when there is a disparity
			-- between the number of bigits in Current and a_other.
		require
			other_exists: a_other /= Void
			simple_inapplicable: count >= karatsuba_threshold or a_other.count >= karatsuba_threshold
			big_size_difference: (count >= karatsuba_threshold and a_other.count >= karatsuba_threshold) implies
									(count - a_other.count).abs > 1
		local
			big, lit, p: like Current
			i: INTEGER
			t: INTEGER		-- block length
			n: INTEGER		-- number of blocks
			e: INTEGER		-- number of left over bigits at high end
			bn: like Current

		do
			Result := new_big_number (zero_bigit)
				-- Find which factor has the most bigits, in order to
				-- multiply the longer number by the shorter number.
			if count < a_other.count then
				lit := Current
				big := a_other
			else
				lit := a_other
				big := Current
			end
				-- Get the length of the shorter number, so that we can
				-- logically divide the longer into blocks of that length.
			t := lit.count
			n := big.count // t
			e := big.count \\ t
			from i := 1
			until i > n
			loop
					-- Starting at the low-order bigits, break the longer
					-- number into segments containing the same number of
					-- bigits as the shorter number.
				bn := big.partition (t * i, t * i - t + 1)
					-- Recursive multiply.
				p := bn * lit
				if not p.is_zero then
					p.shift_left (t * (i - 1))
				end
				Result := Result + p
				i := i + 1
			end
			if e > 0 then
					-- There's one or more bigits remaining at high end that
					-- do not fill an entire block of size `t'.
				check
					not_complete_block: e < t
					one_block_more: i = n + 1
				end
				bn := big.partition (big.count, t * i - t + 1)
				p := bn * lit
				p.shift_left (t * (i - 1))
				Result := Result + p
			end
			if is_same_sign (a_other) then
				Result.set_is_negative (false)
			else
				Result.set_is_negative (true)
			end
		end

	karatsuba_product (a_other: like Current): like Current
			-- Divide and conquer multiplication using Karatsub's algorithm
			-- where the middle term, z1, is calculated as:
			--    (x1 + x0)(y1 + y0) - x1y1 - x0y0
		require
			other_exists: a_other /= Void
			almost_same_size: (count - a_other.count).abs <= 1
			karatsuba_applies: count >= karatsuba_threshold and a_other.count >= karatsuba_threshold
		local
			n: INTEGER
			a, b: like Current
			c, d: like Current
			z2, z1, z0: like Current
		do
				-- Split both numbers into two smaller ones
			n := count.min (a_other.count)
			n := (n // 2) + (n \\ 2)
				-- High & low order bigits of Current
			a := partition (count, n + 1)
			b := partition (n, 1)
				-- High and low order bigits of a_other
			c := a_other.partition (a_other.count, n + 1)
			d := a_other.partition (n, 1)
				-- Create the terms for the multiplication
				--		z2*Base^2 + z1*Base^1 + z0*Base^0
			z2 := a * c
			z0 := b * d
			z1 := (a + b) * (c + d) - z2 - z0
			if not z2.is_zero then
				z2.shift_left (n * 2)
			end
			if not z1.is_zero then
				z1.shift_left (n)
			end
			Result := z2 + z1 + z0
			if is_same_sign (a_other) then
				Result.set_is_negative (false)
			else
				Result.set_is_negative (true)
			end
		end

	bigits_multiplied (a_bigit, a_other: NATURAL_64; a_tuple: TUPLE [high, low: NATURAL_64])
			-- Multiply `a_bigit' and `a_other', resulting in a product and
			-- a carry, which is passed out in `a_tuple'.
			-- The result is passed out in `a_tuple' instead of as Result, in
			-- order avoid creating a new TUPLE on every call to this feature.
			-- This decomposition feature is used by `scalar_multiply'.
		local
			h: INTEGER_32
			a, b, c, d: NATURAL_64
			ac, ad, bc, bd: NATURAL_64
			high, low: NATURAL_64
			car: NATURAL_64
		do
			-- Options:
			--   1)  Cast each bigit to the next higher representation (e.g.
			--       replace a {NATURAL_8} with a {NATURAL_16})  I rejected
			--       this idea, because there is nothing to which to cast a
			--       {NATURAL_64}.
			--   2)  Split each bigit into two bigits of half the size of the
			--       current representation treating the resulting values as the
			--       coefficients of a factored polynomial and multiply:
			--
			--                       a   b
			--           a_bigit =  0111 1111     ==>  Ax + B
			--           a_other =  0111 1111     ==>  Cx + D
			--                       c   d
			--
			--       Evaluating the terms gives:
			--           ac  =  00000111 * 00000111 = 00110001
			--           ad  =  00000111 * 00001111 = 01101001
			--           bc  =  00001111 * 00000111 = 01101001
			--           bd  =  00001111 * 00001111 = 11100001
			--       So, for the multiplication of  (Ax + B)(Cx + D), showing only
			--       the coefficients gives:
			--          ac + (ad + bc) + bd
			--
			--       I rejected this approach, because it is not clear how to
			--       handle the middle term in relation to both the first and
			--       third term.
			--
			--   3)  Multiply the bigits in place, keeping the result as the
			--      `value' and temporarily accepting the lose of any overflow.
			--      Then calculate the lost overflow using an algorithm from
			--      http://stackoverflow.com/questions/28868367/getting-the-
			--      high-part-of-64-bit-integer-multiplication, storing that
			--      overflow in the `carry'.
			--         This bit-shifting approach works for any size representation.
			--
					-- Calculate the low-order half of the Result.
					-- Any overflow is discarded.
			a_tuple.low := a_bigit * a_other
				-- Now calculate the high-order half of the Result.
				-- Find the shift amount
			h := bits_per_bigit // 2
				-- High & low order bits of `a_bigit'
			a := a_bigit.bit_shift_right (h)
			b := a_bigit.bit_shift_left (h).bit_shift_right (h)
				-- High and low order bits of `a_other'
			c := a_other.bit_shift_right (h)
			d := a_other.bit_shift_left (h).bit_shift_right (h)
				-- Calculate the four terms.
			ac := a * c
			ad := a * d
			bc := b * c
			bd := b * d
				--  ((ad + bc) + (bd >> h)) >> h
			car := ((ad.bit_shift_left (h).bit_shift_right (h) +
					bc.bit_shift_left (h).bit_shift_right (h)) +
					bd.bit_shift_right (h)).bit_shift_right (h)
				--  ac + (ad >> h) + (bc >> h) + car
			a_tuple.high := ac + ad.bit_shift_right (h) + bc.bit_shift_right (h) + car
		end

	karatsuba_threshold_imp: INTEGER_32_REF
			-- Implementation of the `karatsuba_threshold', the value above
			-- which multiplication operations use `karatsuba_product'.
		once
			create Result
			Result.set_item (Default_karatsuba_threshold)
		end

feature -- Basic operations (a_other)

	raise (a_power: NATURAL_32)
			-- Raise Current by `a_power'.
			-- This is a slow implementation using a loop.
		local
			n: like Current
		do
			n := power (a_power)
			deep_copy (n)
		end

	power alias "^" (a_power: NATURAL_32): like Current
			-- The result of raising Current to `a_power'.
		local
			p: NATURAL_32
			base: like Current
		do
			if a_power = 0 then
				Result := one
			elseif a_power = 1 then
				Result := Current.deep_twin
			else
				base := Current.deep_twin
				p := a_power
				Result := one
				from
				until p <= 0
				loop
					if p \\ 2 = 0 then
						p := p // 2
						base := base * base
					else
						p := p - 1
						Result := Result * base
					end
				end
			end
		end

	power_modulo (a_power, a_modulus: like Current): like Current
			-- The result of (Current raised to `a_power' modulo `a_modulus'
		require
			not_negative: not is_negative
			positive_modulus: a_power > zero
			odd_modulus: not a_modulus.is_even
		local
			b, p: like Current
			two: like Current
		do
			if a_modulus ~ one then
				Result := zero
			else
				Result := one
				two := one + one
				b := Current \\ a_modulus
				p := a_power.deep_twin
				from
				until p <= zero
				loop
					if p \\ two ~ one then
						Result := (Result * b) \\ a_modulus
					end
					p.bit_shift_right (1)
					b := (b * b) \\ a_modulus
				end
			end
		ensure
			result_small_enough: Result.magnitude < a_modulus.magnitude
		end

--	power alias "^" (a_power: REAL_64): REAL_64
--			-- FIX ME to return a BIG_REAL
--		do
--		end

feature  -- Division

	divide (a_other: like Current)
			-- Modify Current to contain the integer quotient between Current
			-- and `a_other'; discard the remainder.
		require
			divisor_not_zero: a_other /~ zero
		local
			q: like quotient
		do
			q := quotient (a_other)
			copy (q.quot)
		end

	quotient alias "/" alias "÷" (a_other: like Current): TUPLE [quot, rem: like Current]
--	quotient alias "/" (a_other: like Current): c
			-- The quotient and remainder resulting from dividing Current
			-- by `a_other' without changing Current.
		require
			divisor_non_zero: a_other /~ zero
		local
			x, y: like Current
			i, n: INTEGER
		do
				-- Reminder:  the tilde operator calls `is_less'.
			if Current ~ zero then
					-- Numerator is zero.
				Result := [zero, zero]
			elseif count < a_other.count or else magnitude < a_other then
					-- Denominator is bigger than numerator.
				Result := [zero, Current.twin]
			elseif a_other.is_one then
					-- Dividing by one or negative one.
				if a_other ~ one then
					Result := [Current.twin, zero]
				else
					Result := [Current.twin.opposite, zero]
				end
			elseif Current.is_magnitude_equal (a_other) then
					-- Absolute values of numerator and denominator are equal.
				if Current.is_same_sign (a_other) then
					Result := [one, zero]
				else
					Result := [one.opposite, zero]
				end
			elseif count = 1 then
				check
					other_also_length_one: a_other.count = 1
						-- because of the second condition above.
				end
				Result := [new_big_number (Current.i_th (1) // a_other.i_th (1)),
							new_big_number (Current.i_th (1) \\ a_other.i_th (1))]
			else
					-- Do calculations with positive numbers...
					-- to be corrected below
				x := magnitude
				y := a_other.magnitude
				i := y.normalize
				n := 0
				x.bit_shift_left (i)
				if x.count = 2 and y.count = 1 then
					Result := divide_two_bigits_by_one (x.i_th (2), x.i_th (1), y.i_th (1))
					-- Count is even and twice the count of a_other.
				elseif y.count <= div_limit then
					Result := school_divide (x, y)
				else
					x.set_unstable
					y.set_unstable
					n := x.condition_for_x_by_one_division (y)
					Result := x.x_by_one_divide (y)
				end
				Result.rem.bit_shift_right (i)
				Result.rem.shift_right (n)
				Result.rem.remove_leading_zeros
				Result.quot.remove_leading_zeros
				Result.rem.set_stable
				Result.quot.set_stable
			end
				-- Correct the signs of the answer.
				-- Remember, calculations are done with magnitude only
			if not is_same_sign (a_other) then
				Result.quot.set_is_negative (true)
			end
			if is_negative then
				Result.rem.set_is_negative (true)
			end
		ensure
			valid_result: (Result.quot * a_other + Result.rem) ~ Current
			remainder_large_enough: Result.rem.magnitude >= zero
			remainder_small_enough: Result.rem.magnitude < a_other.magnitude
		end

	integer_quotient alias "//" (a_other: like Current): like Current
			-- Integer division by `a_other', discarding any remainder.
			-- Does not change Current.
		do
			Result := quotient (a_other).quot
		end

	integer_remainder alias "\\" (a_other: like Current): like Current
			-- Remainder of the integer division of Current by `a_other'.
			-- Does not change Current.
		do
			Result := quotient (a_other).rem
		ensure
			result_small_enough: Result.magnitude < a_other.magnitude
		end

	scalar_quotient_with_remainder (a_bigit: NATURAL_64): TUPLE [quot, rem: like Current]
			-- Divide Current by `a_bigit' giving a quotient and remainder.
			-- Basic division of a number by one bigit.
			-- Complexity = O(n).
		require
			non_zero_divisor: a_bigit /= zero_bigit
		local
			denom: like Current
		do
			denom := get_number (a_bigit)
			Result := quotient (denom)
			put_number (denom)
		ensure
			same_signs_implication: (is_negative and a_bigit < zero_bigit) or
									(not is_negative and a_bigit >= zero_bigit) implies
										 not Result.quot.is_negative
			different_signs_implication: (is_negative and a_bigit < zero_bigit) or
										(not is_negative and not (a_bigit >= zero_bigit)) implies
										 Result.quot.is_negative
			definition: Result.quot.scalar_product (a_bigit) + Result.rem ~ Current
		end

feature {JJ_BIG_NUMBER_64} -- Implementation (division)

	div_limit_imp: INTEGER_32_REF
			-- Implementation of the `div_limit', the denominator bigit count
			-- above which division uses Burnikel and Zieler recursive division.
		once
			create Result
			Result.set_item (Default_div_limit)
		end

	school_divide (a, v: like Current): TUPLE [quot, rem: like Current]
			-- The quotient and remainder of dividing `a' by `v', implemented as
			-- described by Knuth's Algorithm D.
			-- See Knuth, "The Art of Computer Programming", Vol 2, second edition,
			-- pp 257-258.
		require
			n_greater_than_one: v.count >= 1
			a_long_enough: a.count >= v.count
			is_normalized: v.is_normalized
		local
			j: INTEGER
			n, m: INTEGER
			b: like Current
			u: like Current
			v1, v2, u1, u2: NATURAL_64
			u12: like Current
			r: like Current				-- a remainder
			q, q_hat, d: like Current
--			tup: like divide_two_bigits_by_one
		do
			if a.is_zero then
				Result := [new_big_number (zero_bigit), new_big_number (zero_bigit)]
			else
				n := v.count
				m := a.count
				b := ones (n)
				q := new_big_number (zero_bigit)
				u := new_big_number (zero_bigit)
				v1 := v.i_th (v.count)
				if v.count > 1 then
					v2 := v.i_th (v.count - 1)
				else
					v2 := zero_bigit
				end
					-- D1.  Normalize.  It already is normalized.
				check
					already_normalized: v.is_normalized
						-- because of precondition.  Must assume `a' was adjusted.
				end
				if a.count = v.count then
					a.extend (zero_bigit)
				end
					-- Start at high-order bigit.
					-- Feature `new_sub_number' arguments:  (a_low, a_high).
					-- One-based arrays make the indexing look odd, but...
					-- Get `n' number of bigits or at least two.
				u := a.partition (a.count, a.count - n + 1)
				from j := a.count - n	-- m - n
				until j = 0
				loop
						-- Get one more bigit.
					u.insert (a.i_th (j), 1)
					from
					until u.count > v.count
					loop
						u.extend (zero_bigit)
					end
					u1 := u.i_th (u.count)
					u2 := u.i_th (u.count - 1)
						-- D3.  Calulate `q_hat'.
	--				if u1 = v1 then
	--					q_hat := new_big_number (max_bigit)
	--				else
						q_hat := divide_two_bigits_by_one (u1, u2, v1).quot
						if v.count > 1 then
							u12 := get_number (u1)
							u12.insert (u2, 1)
							d := q_hat.scalar_product (v2)
							r := u12 - q_hat.scalar_product (v1)
							r.insert (u.i_th (u.count - 2), 1)
							if d > r then
								q_hat := q_hat - one
								d := q_hat.scalar_product (v2)
								r := u12 - q_hat.scalar_product (v1)
								r.insert (a.i_th (a.count - 2), 1)
								if d > r then
									q_hat := q_hat - one
								end
							end
							put_number (u12)
						end
	--				end
						-- D4.  Multiply and subtract.
					u := u - (q_hat * v)
						-- D5.  Test remainder.
					if u.is_negative then
							-- D6.  Add back.
						q_hat := q_hat - one
						u := u + v
					end
					check
						q_hat_count_big_enough: q_hat.count >= 1
						q_hat_count_small_enough: q_hat.count <= 2
					end
					if j < a.count - n then
							-- Shift if NOT the first time through loop.
						q.shift_left (q_hat.count)
					end
					q.add_imp (q_hat)
					j := j - 1
				end
				Result := [q, u]
			end
		ensure
			valid_result: Result.quot * v + Result.rem ~ a
		end

	two_by_one_divide (a, a_other: like Current): TUPLE [quot, rem: like Current]
			-- The result of dividing a number that has twice the number of
			-- bigits as `a_other'.  The number is split into its high-order
			-- and low-order bigits, `a_high' and `a_low'.
			-- See Burnikel & Zieler, "Fast Recursive Division", p 4,
			-- Algorighm 1.
		require
			divisor_is_normalized: a_other.is_normalized
			n_big_enough: a_other.count >= div_limit or a_other.count = 1
			n_is_even_or_one: a_other.count \\ 2 = 0
			is_twice_n: a.count = 2 * a_other.count
		local
			a12, a3, a4: like Current
			r1, r2: like Current
			n, half_n, t: INTEGER
			q: like Current
			tup: TUPLE [quot, rem: like Current]

			test_q, test_r, test_x: like Current
		do
			if a.is_zero then
				Result := [new_big_number (zero_bigit), new_big_number (zero_bigit)]
			elseif a_other.count = 1 then
				check
					should_not_happen: false
						-- because ?  remove if never reached.
				end
				Result := divide_two_bigits_by_one (a.i_th ((2)), a.i_th (1), a_other.i_th (1))
--				Result := divide_two_bigits_by_one (a, a_other)
			else
				n := a_other.count
				half_n := n // 2
					-- 2)  Split A into four parts...
					-- High order bigits first
					-- Reminder:  `t_th_block' counts blocks from low to high.
				a12 := a.t_th_block (2, a.count // 2)
				t := a.count // 4
				a3 := a.t_th_block (2, t)
				a4 := a.t_th_block (1, t)
					-- 3) Compute the high part Q1 of floor(A/B)... with remainder...
				tup := three_by_two_divide (a12, a3, a_other)
				q := tup.quot
				if q ~ zero then
					do_nothing
				end
					-- 4) Compute the low part Q2...
				from
				until tup.rem.count >= a_other.count
				loop
					tup.rem.extend (zero_bigit)
				end
				check
					rem_has_correct_count: tup.rem.count = a_other.count
				end
				test_q := new_big_number (two_bigit * ten_bigit * ten_bigit + ten_bigit + two_bigit)
				if tup.quot ~ test_q then
					do_nothing
				end
				tup := three_by_two_divide (tup.rem, a4, a_other)
					-- Debugging ... remove me.
					-- Just reusing `a3' and `r1' as variables.
				if tup.quot ~ new_big_number (sixteen_bigit) then
					do_nothing
				end
				test_r := new_big_number (ten_bigit * ten_bigit + ten_bigit * three_bigit + seven_bigit)
				test_x := new_big_number (two_bigit * ten_bigit + nine_bigit)
				test_x.shift_left (1)
					-- Call `add_imp' not `add' in order to preserve leading zeros.
				test_x.add_imp (test_r)
				if a4 ~ test_x then
					do_nothing
				end
				if tup.quot ~ zero then
					do_nothing
				end
				Result := [new_combined_number (q, tup.quot), tup.rem]
			end
		ensure
			valid_result: Result.quot * a_other + Result.rem ~ a
		end

	three_by_two_divide (a, a3, b: like Current): TUPLE [quot, rem: like Current]
			-- Called by `two_by_one_divide'.  It has similar structure as
			-- `div_three_halves_by_two_halfs', but the arguments to this
			-- function have type {JJ_BIG_NATURAL} instead of like `bigit'.
			-- See Burnikel & Zieler, "Fast Recursive Division", pp 4-8,
			-- Algorithm 2.
		require
			b_big_enough: b.count >= div_limit
			n_not_odd: b.count \\ 2 = 0
			b_has_2n_bigits: b.count = a3.count * 2
			a_has_2n_bigits: a.count = a3.count * 2
		local
			n: INTEGER
			is_a_too_small: BOOLEAN
			new_a, a1, a2: like Current
			b1, b2: like Current
			tup: TUPLE [quot, rem: like Current]
			q, q1, q2, r, r1: like Current
			c, d: like Current
		do
			if a.is_zero and a3.is_zero then
				Result := [new_big_number (zero_bigit), new_big_number (zero_bigit)]
			else
				n := b.count // 2
					-- 1) Split `a'.
				a1 := a.partition (a.count, n + 1)
				a2 := a.partition (n.max (1), 1)
					-- 2) Split `b'.
				b1 := b.partition (b.count, n + 1)
				b2 := b.partition (n.max (1), 1)
					-- 3) Distinguish cases.
					-- 3b) We know that the quotient will contain a one
					-- in the first bigit.  Remember this and adjust `a'
					-- to meet B&S's precondition by subracting `b' from `a'.
				if a1 >= b1 then
					is_a_too_small := true
					new_a := a - b
					check
						new_a.count = 2 * n
							-- because must preserve leading zeroes.
					end
						-- 1) Split the `new_a'.
					a1 := new_a.partition (new_a.count, n + 1)
					a2 := new_a.partition (n.max (1), 1)
				else
					new_a := a
				end
					-- At this point, `a' MUST be less than 'b', so continue
					-- with step 3a).
				check
					a_now_small_enough: a1 < b1
						-- because we just subtracted the normalized denominator.
				end
					-- 3a) compute Q = floor ([A1,A2] / B1 with remainder.
				if b1.count < div_limit then
					tup := school_divide (new_a, b1)
				else
					tup := two_by_one_divide (new_a, b1)
				end
				q := tup.quot
				r1 := tup.rem
					-- 4) D = Q * B2
				d := q * b2
					-- 5) R1 * Beta^n + A3 - D.  (The paper says "a4".)
				r1.shift_left (n)
				r := r1 + a3 - d
					-- 6) As long as R < 0, repeat
				from
				until not r.is_negative
				loop
					r := r + b
					q.decrement
				end
				check
					remainder_small_enough: r.count <= b.count
						-- because remainder must be less than divisor.
				end
				if is_a_too_small then
					q.extend (one_bigit)
				end
				Result := [q, r]
				if q ~ zero then
					do_nothing
				end
			end
		ensure
--			n_bigit_remainder: Result.rem.count = b.count // 2
			quotient_has_correct_count: Result.quot.count <= b.count
			valid_result: Result.quot * b + Result.rem ~ new_combined_number (a, a3)
		end

	x_by_one_divide (a_other: like Current): TUPLE [quot, rem: like Current]
			-- The result of dividing Current by `a_other' when Current
			-- has x number of blocks of bigits, where each block has the
			-- number of bigits in `a_other'.
			-- See Burnikel & Zieler, "Fast Recursive Division", pp 9-10.
		require
			not_needless_call: not is_zero
			not_divide_by_zero: not a_other.is_zero
			other_long_enough: a_other.count > 0
			other_has_power_of_two_count: a_other.count - (a_other.count - 1) = 1
			other_is_normalized: a_other.is_normalized
			long_enough: count > a_other.count
			has_correct_count: count \\ a_other.count = 0
		local
			i, t, n, low, high: INTEGER
			sft: INTEGER
			a, z: like Current
			q_sub_i: TUPLE [quot, rem: like Current]
			q: like Current
		do
			n := a_other.count
				-- Get the number of blocks into which Current is divided.
			t := count // n
			q := zero
			q_sub_i := [zero, zero]
				-- Get the first two blocks.
			a := t_th_block (t, n)
			z := t_th_block (t - 1, n)
			from i := t - 1
			until i = 0
			loop
					-- Combine the next block `z' (low-order) with the
					-- remainder `a' (high-order).
				z.combine (a)
				q_sub_i := two_by_one_divide (z, a_other)
					-- Ensure the remainder has enough bigits for `two_by_one_divide'.
				from
				until q_sub_i.rem.count >= a_other.count
				loop
					q_sub_i.rem.extend (zero_bigit)
				end
				if q.is_zero then
					q.add (q_sub_i.quot)
				else
					if q_sub_i.quot ~ zero then
						do_nothing
					end
					q := new_combined_number (q, q_sub_i.quot)
				end
				a := q_sub_i.rem
					-- Bring down next block.
				if i > 1 then
					z := t_th_block (i - 1, n)
				end
				i := i - 1
			end
			Result := [q, q_sub_i.rem]
			Result.quot.remove_leading_zeros
		ensure
			valid_result: Result.quot * a_other + Result.rem ~ Current
		end

	t_th_block (t: INTEGER; a_size: INTEGER): like Current
			-- Get a new number consisting of the t-th block of bigits
			-- where `a_size' is the number of bigits in each block.
			-- Index `t' begins with block one at the low-order end
			-- and increases toward the high-order end.
			-- Decomposition feature used by `x_by_one_divide'.
			-- See Burnikel & Zieler, "Fast Recursive Division", pp 9-10.
		require
			t_big_enough: t >= 1
			size_big_enough: a_size >= 1
			valid_block_size: count \\ a_size = 0
			valid_block: t <= count // a_size
		local
			high, low: INTEGER
		do
			high := t * a_size
			low := high - a_size + 1
			Result := partition (high, low)
		ensure
			result_has_correct_count: Result.count = a_size
		end

	condition_for_x_by_one_division (a_other: like Current): INTEGER
			-- Modify, if required, both Current and `a_other' so that a
			-- left-to-right division using `two_by_one_divide' is possible,
			-- returning the amount of left shifting required.
			-- This feature ensures `a_other' has a power-of-two number of
			-- bigits shifting `a_other' to the left (i.e. adding low-order
			-- zeroes).  Current is shifted the same amount.  Finally,
			-- Current is padded with zeroes on the high-order end so that
			-- it has a number of bigits that is a multiple of the number of
			-- bigits now in `a_other'.
			-- See Burnikel & Zieler, "Fast Recursive Division", pp 9-10.
			-- The Result is the number of left shifts, which we later use to
			-- fix the remainder given by `x_by_one_divide', because the remainder
			-- returned by that function is "the overall division remainder
			-- (left-shifted by 2^alpha)."
			-- See Burnikel & Zieler, page 10, just above "Claim 2".
		require
			is_marked_unstable: is_unstable
			other_is_marked_unstable: a_other.is_unstable
		local
			n: INTEGER		-- number of left shifts after normalization
		do
				-- Extend the divisor (i.e. `a_other') so that it has 2^k (i.e. a
				-- power of two) bigits by adding zeros on the low-order end
				-- and adjust Current accordingly.
			from n := 0
			until a_other.count.bit_and (a_other.count - 1) = 0
			loop
				a_other.put_front (zero_bigit)
				n := n + 1
			end
				-- Now shift Current so it has the same number of "new" zeroes.
			shift_left (n)
			Result := n
				-- Normalize `a_other' and adjust Current accordingly.
			n := a_other.normalize
			bit_shift_left (n)
				-- Pad Current with leading zeroes to make its bigit `count'
				-- a multiple of `a_other'.
			if count = a_other.count then
				extend (zero_bigit)		-- special case
			end
			from
			until count \\ a_other.count = 0
			loop
				extend (zero_bigit)
			end
		ensure
			divisor_count_power_of_two: a_other.count.bit_and (a_other.count - 1) = 0
			count_is_multiple_of_other_count: count \\ a_other.count = 0
			other_is_normalized: a_other.is_normalized
		end

	divide_two_bigits_by_one (a_high, a_low, a_divisor: NATURAL_64):
							TUPLE [quot: like Current; rem: like Current]
			-- Divide a two-bigit number containing `a_high' and `a_low' bigit
			-- by a single-bigit number `a_divisor', returning a quotient and remainder.
			-- See "Fast Recursive Division" by Burnikel and Ziegler, p 3.
		require
			divisor_non_zero: a_divisor /= zero_bigit
			divisor_is_normalized: is_bigit_normalized (a_divisor)
		local
			i: INTEGER
			tup: TUPLE [a3, a4: NATURAL_64]
			qr: like div_three_halves_by_two
			qs: like div_three_halves_by_two
			r: TUPLE [r1, r2: NATURAL_64]
			f: NATURAL_64
			quot, rem: like Current
		do
				-- "Let [a3, a4] = AL"
			tup := as_half_bigits (a_low)
				-- "[q1,R] = DivThreeHalvesByTwo (a1,a2,a3,b1,b2)"
			qr := div_three_halves_by_two (a_high, tup.a3, a_divisor)
				-- "Let [r1,r2] = R"
			r := as_half_bigits (qr.rem)
				-- "[q2,S] = DivThreeHalvesByTwo (r1,r2,a4,b1,b2)"
			qs := div_three_halves_by_two (qr.rem, tup.a4, a_divisor)
				-- "Return the quotient Q = [q1,q2] and the remainder S."
			if qr.quot > max_half_bigit then
				quot := new_big_number (qr.quot)
				quot.bit_shift_left (bits_per_bigit // 2)
				quot.scalar_add (qs.quot)
			else
				f := as_full_bigit (qr.quot, qs.quot)
				quot := new_big_number (f)
			end
			rem := new_big_number (qs.rem)
			Result := [quot, rem]
		ensure
			quotient_short_enough: Result.quot.count <= 2
			result_checks: Result.quot.scalar_product (a_divisor) + Result.rem ~
							new_big_number (a_high).shifted_left (1).scalar_sum (a_low)
		end

	div_three_halves_by_two (A, a3, B: NATURAL_64): TUPLE [quot, rem: NATURAL_64]
			-- Divide three half-bigits, "a1" and "a2" (contained in `A') and
			-- `a3' by two half-bigits, "b1" and "b2" (contained in `B'), in
			-- such a way that the computations fit into the representation
			-- (i.e. the number of bits) of a bigit.
			-- See "Fast Recursive Division" by Burnikel and Ziegler, p 3 to top p 4.
		local
			tup: TUPLE [b1, b2: NATURAL_64]
			q, c, ca: NATURAL_64
			D, R: JJ_BIG_NUMBER_64
			t: NATURAL_64
		do
			tup := as_half_bigits (B)
			q := a // tup.b1		-- Line 6
			c := a \\ tup.b1		-- Same as "c := A - Q * b1" (i.e. remainder).
				-- Use a big number to account for values larger than one bigit.
			D := get_number (q)
			D.scalar_multiply (tup.b2)	-- Same as "d := q * b2"
				-- Add half-bigit of numerator to remainder.
			ca := as_full_bigit (c, a3)	-- Same as [c,a3]
				-- Perform "R := [c,a3] - D", line 9
			D.negate
			R := D.scalar_sum (ca)
				-- We are done with `D'.
			put_number (D)
				-- Here begins the code that corrects a quotient that is too big.
				-- Unlike Bernikel and Ziegler's algorithm, which makes at most
				-- two corrections to the originally "guessed" quotient, this code,
				-- for some reason, can make up to four corrections.  Most of the
				-- time, only one or no corrections are made.  Tests on possible
				-- values in 8-bit representation shows this distribution:
				--   num corrections     percentage
				--          0              52.32%
				--          1              40.77%
				--          2               6.15%
				--          3               0.75%
				--          4               0.01%
			from
			until not R.is_negative
			loop
				q := q - one_bigit
				R.scalar_add (B)
			end
			if r.count = 0 then
					-- Add a zero to make Result easier to obtain.
				r.extend (zero_bigit)
			end
			check
				only_one_bigit: R.count <= 1
			end
			Result := [q, R.i_th (1)]
		end

	as_half_bigits (a_bigit: NATURAL_64): TUPLE [high, low: NATURAL_64]
			-- Split `a_bigit' into two half-bigits.
			-- See "Fast Recursive Division" by Burnikel and Ziegler, p 3.
		local
			h: INTEGER
		do
			h := bits_per_bigit // 2
			Result := [a_bigit.bit_shift_right (h),
						a_bigit.bit_shift_left (h).bit_shift_right (h)]
		end

	as_full_bigit (a_high, a_low: NATURAL_64): NATURAL_64
			-- Combine `a_high' half and `a_low' half into a single bigit.
		require
			high_small_enough: a_high <= max_half_bigit
			low_small_enough: a_low <= max_half_bigit
		do
			Result := a_high.bit_shift_left (bits_per_bigit // 2) + a_low
		end

	normalize: INTEGER
			-- Ensure Current `is_normalized', possibly shifting the bits of
			-- each bigit in Current until the most high-order bigit is greater
			-- or equal to the `base' // 2, returning the number of shifts
			-- required to do so.
			-- Knuth: The Art of Computer Programming, Volumn 2", pp 257-258.
		require
			not_zero: magnitude > zero
		local
			n: INTEGER
			c, c_out: NATURAL_64
			d: NATURAL_64
			i: INTEGER
		do
			Result := bits_per_bigit - most_significant_bit (i_th (count))
			bit_shift_left (Result)
		ensure
			is_normalized: is_normalized
			unchanged_count: count = old count
		end

	most_significant_bit (a_value: NATURAL_64): INTEGER
			-- The index of the most significant bit that is set.
			-- O(n) where n is the number of bits.
			-- Helper function for `normalize'
		local
			n: NATURAL_64
			i: INTEGER
		do
				-- Naive approach, but not the same as `log_base_two'
				-- as implied in "Bit Twiddling Hacks" at
				-- https://graphics.stanford.edu/~seander/bithacks.html.
				-- S. Anderson's `log_base_two' assumes zero-based indexing
				-- and will be one off for bit position (and does not apply
				-- to log(0), because it gives zero as answer even though
				-- none of the bits are set.
			from n := a_value
			until n <= n.zero
			loop
				Result := Result + 1
				n := n.bit_shift_right (1)
			end
		ensure
			zero_result_definition: Result = zero implies Current = zero
			result_small_enough: Result <= bit_count
		end

	is_normalized: BOOLEAN
			-- Is the most high-order bigit greater than or equal to `base' // 2?
			-- (See Knuth: The Art of Computer Programming, Volumn 2".)
		do
			Result := i_th (count) >= max_bigit // two_bigit + one_bigit
		ensure
			definition: Result implies i_th (count) >= max_bigit // two_bigit + one_bigit
		end

	is_bigit_normalized (a_bigit: NATURAL_64): BOOLEAN
			-- Is `a_bigit' greater than or equal to `base' // 2?
			-- (See Knuth: The Art of Computer Programming, Volumn 2".)
		do
			Result := a_bigit >= max_bigit // two_bigit + one_bigit
		end

	bit_shift_left (a_shift: INTEGER_32)
			-- Change Current by shifting the bits left by `a_number', carrying
			-- into the next bigit if required.
		require
			shift_big_enough: a_shift >= 0
			shift_small_enough: a_shift <= bits_per_bigit
		local
			c, c_out: NATURAL_64
			d: NATURAL_64
			i: INTEGER
		do
			if a_shift > 0 then
				c := zero_bigit
				from i := 1
				until i > count
				loop
						-- Determine the carry out
					c_out := i_th (i).bit_shift_right (bits_per_bigit - a_shift)
						-- Shift to far to zero out unutilized bits.
					d := i_th (i).bit_shift_left (a_shift)
						-- Shift back to prepare for modulo add
					d := d.bit_shift_right (bits_per_bigit - bits_per_bigit)
						-- Modulo add the carry
					put_i_th (d + c, i)
					c := c_out
					i := i + 1
				end
				if c > zero_bigit then
					extend (c)
				end
			end
		ensure
			count_might_grow: count <= old count + 1
		end

	bit_shift_right (a_shift: INTEGER_32)
			-- Change Current by shifting the bits right by `a_number', carrying
			-- into the lower-order bigit if required.
			-- Bits could be lost as they are shifted off the end.
		require
			shift_big_enough: a_shift >= 0
		local
			c, c_down: NATURAL_64
			d: NATURAL_64
			i: INTEGER
			n: INTEGER
		do
			if a_shift > 0 then
				c := zero_bigit
				n := bits_per_bigit - a_shift
				from i := count
				until i < 1
				loop
						-- Determine the carry down.
					c_down := i_th (i).bit_shift_left (n).bit_shift_right (n)
					c_down := c_down.bit_shift_left (n)
						-- Shift the i-th bigit.
					d := i_th (i).bit_shift_right (a_shift)
						-- Add any carry down bits.
					put_i_th (d + c, i)
					c := c_down
					i := i - 1
				end
				if count > 1 and then i_th (count) ~ zero_bigit then
					go_i_th (count)
					remove
				end
			end
		ensure
			count_might_shrink: count >= old count - 1
		end

feature -- Comparison

feature -- Output

	out: STRING_8
			-- Representation as a base ten number with NO coma seperators.
		local
			len: INTEGER
			n, q, r: like Current
			quot: like quotient
			ten_n: like Current	-- a multiple of 10
			s: STRING
		do
			Result := ""
			len := max_ten_power.out.count - 1	-- number of zeros ?
			ten_n := get_number (max_ten_power)
			from n := magnitude		-- so its always positive.
			until n ~ zero
			loop
				quot := n / ten_n
				q := quot.quot
				r := quot.rem
				check
--					only_one_bigit_in_remainder: r.count = 1
				end
				s := r.i_th (1).out
				from
				until s.count = len
				loop
					s.prepend ("0")
				end
				Result.prepend (s)
				n := q
			end
			Result.prune_all_leading ('0')
			if Result.count = 0 then
				Result := "0"
			end
			if is_negative then
				Result.prepend ("-")
			end
				-- Done with `ten_n'.
			put_number (ten_n)
		end

	out_formatted: STRING_8
			-- Representation as base ten number with coma's.
		local
			len: INTEGER
			p, q, r: like Current
			quot: like quotient
			ten_n: like Current	-- a multiple of 10
			s: STRING_8
			i, n: INTEGER_32
		do
			s := out
			create Result.make (s.count)
			if s.count > 3 then
				from i := s.count
				until i <= 3
				loop
					Result.prepend ("," + s.substring (i - 2, i))
					i := i - 3
				end
				Result.prepend (s.substring ((i-2).max (1), i))
			else
				Result := s
			end
		end

	out_as_stored: STRING_8
			-- Output as sequence of bigits seperated by comas.
			-- Example output of an 8-bit (i.e. base-256) representation
			-- of the number 196,353 is "<2,3,255,1>", which is the same
			-- as (2*256^3) + (3*256^2) + (255*256^1) + (1*256^0).
		local
			i, j: INTEGER
			s: STRING
			n: INTEGER
		do
				-- Determine the length of string needed to represent
				-- the `largest_bigit'.
			s := max_bigit.out
			n := s.count
			create Result.make (count * n)
			if is_negative then
				Result.append ("-")
			end
			Result.append ("<")
			from i := count
			until i < 1
			loop
				s := i_th (i).out
				Result.append (s)
				if i > 1 then
					Result.append (",")
				end
				i := i - 1
			end
			Result.append (">")
		end

	out_as_bits: STRING_8
			-- Output as groups of bits.
		local
			i: INTEGER
			j: like bits_per_bigit
			dig: NATURAL_64
			bc: INTEGER_32
			n: INTEGER
		do
			create Result.make_empty
			if is_negative then
				Result := "-"
			end
			bc := bits_per_bigit
			from i := count
			until i < 1
			loop
				dig := i_th (i)
				from j := 1
				until j > bc
				loop
					n := bc - j + 1					-- which bit?
					if dig.bit_test (n - 1) then		-- zero base test
						Result.append ("1")
					else
						Result.append ("0")
					end
					j := j + 1
				end
				if i > 1 then
					Result.append (",")
				end
				i := i - 1
			end
		end

feature {JJ_BIG_NUMBER_64} -- Implementation

	shift_left (a_shift: INTEGER)
			-- Shift the bigits to the left by putting zeros into the
			-- low-order bigits.
		require
			shift_big_enough: a_shift >= 0
		local
			i: INTEGER
		do
			from i := 1
			until i > a_shift
			loop
				insert (zero_bigit, 1)
				i := i + 1
			end
		end

	shift_right (a_shift: INTEGER)
			-- Shift the bigits to the right by dropping the
			-- low-order bigits.
		require
			shift_big_enough: a_shift >= 0
		local
			i: INTEGER
		do
			go_i_th (1)
			from i := 1
			until i > a_shift
			loop
				remove
				i := i + 1
			end
		end

	shifted_left (a_shift: INTEGER): like Current
			-- The result of shifting Current to the left by `a_shift'
			-- without changing Current.
		require
			shift_big_enough: a_shift >= 0
		do
			Result := twin
			Result.shift_left (a_shift)
		end

	is_nonconforming: BOOLEAN
			-- Is Current NOT in the correct format?
			-- Yes, if there is a leading zero when `count' > 1.
		do
			Result :=  count > 1 and then i_th (count) = zero_bigit
		end

	is_unstable: BOOLEAN
			-- Used internally to avoid an invariant violation when a
			-- selectively exported feature would otherwise violate
			-- the invariant (e.g. `scalar_divide' might put a leading
			-- zero into Current).

	set_stable
			-- Ensure not `is_unstable'.
		do
			is_unstable := false
		end

	set_unstable
			-- Ensure `is_unstable'.
		do
			is_unstable := true
		end

	remove_leading_zeros
			-- Remove any leading zeros.
		do
			from
			until count = 1 or else i_th (count) > zero_bigit
			loop
				go_i_th (count)
				remove
			end
		end

	combine (a_high: like Current)
			-- Extend the bigits of `a_high' into Current as
			-- Current's new high-order bigits.
		local
			i: INTEGER
		do
			from i := 1
			until i > a_high.count
			loop
				extend (a_high.i_th (i))
				i := i + 1
			end
		end

	prepend_other (a_other: like Current)
			-- Put the bigits of `a_other' into Current as
			-- Current's new low-order bigits.
			-- Same code as used for `merge_right' except this feature
			-- first moves the cursor before (to add at the beginning of
			-- the list) and does not `wipe_out' `a_other'.
		local
			l_new_count, l_old_count: INTEGER
		do
			check
				a_other.count >= 1
					-- because of invariant requiring at least one bigit.
			end
			l_old_count := count
			l_new_count := l_old_count + a_other.count
			if l_new_count > area_v2.capacity then
				area_v2 := area_v2.aliased_resized_area (l_new_count)
			end
			area_v2.insert_data (a_other.area_v2, 0, 1, a_other.count)
		end

	partition (a_high, a_low: INTEGER): like Current
			-- Copy of the bigits indexed from `a_low' up to `a_high' inclusive.
		require
			low_big_enough: a_low >= 1
			high_small_enough: a_high <= count
			low_before_high: a_low <= a_high
		local
			i: INTEGER
		do
			Result := new_big_number (zero_bigit)
			Result.put_i_th (i_th (a_low), 1)
				-- Loop through the rest of the bigits.
			from i := a_low + 1
			until i > a_high
			loop
				Result.extend (i_th (i))
				i := i + 1
			end
				-- Set to same sign as Current, unless `is_zero'.
			if not is_zero then
				Result.set_is_negative (is_negative)
			end
		end

feature {NONE} -- Implementation

	get_number (a_value: NATURAL_64): like Current
			-- Get a {JJ_BIG_NATURAL} from the `number_cache' or, if
			-- required, create a new one, placing `a_value' in it.
		do
			if number_cache.is_empty then
				Result := new_big_number (a_value)
			else
				Result := number_cache.item
				Result.set_with_value (a_value)
				number_cache.remove
			end
		end

	put_number (a_number: like Current)
			-- Put `a_number' back into the `number_cache', making it
			-- available for use later.
		require
			not_has_number: not number_cache.has (a_number)
		do
			number_cache.extend (a_number)
		end

	number_cache: LINKED_STACK [JJ_BIG_NUMBER_64]
			-- A collection of previously created numbers from which to
			-- select to reduce the number of creations.
			-- I would like this to be defined in {JJ_BIG_NATURAL} as a
			-- once feature, but a once feature can not have an anchored
			-- result.
		once
			create Result.make
		end

	new_big_number (a_value: NATURAL_64): JJ_BIG_NUMBER_64
			-- Factory method to create an instance equivalent to `a_value'.
			-- Wraps the creation feature `from_value'.
		do
			if number_cache.is_empty then
				create Result.from_value (a_value)
			else
				Result := number_cache.item
				Result.set_with_value (a_value)
				number_cache.remove
			end
		end

	new_combined_number (a_high, a_low: JJ_BIG_NUMBER_64): JJ_BIG_NUMBER_64
			-- A new number made from two others where `a_high' contains the
			-- high-order bigits and `a_low' contains the low-order bigits.
		local
			i: INTEGER
		do
			Result := a_low.twin
			Result.combine (a_high)
		end

	new_filled_list (n: INTEGER): JJ_BIG_NUMBER_64
			-- New list with `n' elements.
		do
			Result := new_big_number (zero_bigit)
		end

	new_value_from_character (a_character: CHARACTER_8): NATURAL_64
			-- Get the number given by `a_character'
		do
			Result := zero_bigit
			inspect a_character
			when '0' then
				Result := zero_bigit
			when '1' then
				Result := one_bigit
			when '2' then
				Result := two_bigit
			when '3' then
				Result := three_bigit
			when '4' then
				Result := four_bigit
			when '5' then
				Result := five_bigit
			when '6' then
				Result := six_bigit
			when '7' then
				Result := seven_bigit
			when '8' then
				Result := eight_bigit
			when '9' then
				Result := nine_bigit
			when 'a', 'A' then
				Result := ten_bigit
			when 'b', 'B' then
				Result := ten_bigit + one_bigit
			when 'c', 'C' then
				Result := ten_bigit + two_bigit
			when 'd', 'D' then
				Result := ten_bigit + three_bigit
			when 'e', 'E' then
				Result := ten_bigit + four_bigit
			when 'f', 'F' then
				Result := ten_bigit + five_bigit
			else
				check
					should_not_happen: false
						-- because of precondition
				end
			end
		end

	random_string (a_count: INTEGER): STRING_8
			-- A string of up to `a_count' numbers
			-- Used by `make_random_with_digit_count'
		require
			count_large_enough: a_count >= 1
		local
			done: BOOLEAN
			r: NATURAL_64
		do
			create Result.make_empty
			random.set_range (one_bigit, nine_bigit)
			r := random.item
			Result := Result + random.item.out
			random.forth
				-- Now build a string of random digits
				-- Add the first number which is not a zero
			random.set_range (zero_bigit, nine_bigit)
			from
			until Result.count >= a_count -- or done
			loop
				Result := Result + random.item.out
				random.forth
					-- Finish with number possibly shorter than `a_count'
--				done := random.item > n.eight_bigit
--				random.forth
			end
		ensure
			correct_length: Result.count = a_count
			no_leading_zeros: not (Result.item (1) ~ '0')
		end


	ten_to_the_power (a_power: JJ_BIG_NUMBER_64): JJ_BIG_NUMBER_64
			-- Memoized result of raising 10 to `a_power'.
			-- Helper function for `from_string'.
		require
			positive_exponent: not a_power.is_negative
		local
			num: detachable like Current
			ten: like Current
			p: like Current
		do
			num := power_of_ten_table.item (a_power)
			if not attached num then
				if a_power ~ zero then
					num := new_big_number (one_bigit)
					power_of_ten_table.extend (num, zero)
				elseif a_power ~ one then
					if not power_of_ten_table.has (zero) then
						num := ten_to_the_power (zero)
					end
					num := new_big_number (ten_bigit)
					power_of_ten_table.extend (num, one)
				else
					ten := new_big_number (ten_bigit)
					num := ten_to_the_power (a_power - one) * ten
					power_of_ten_table.extend (num, a_power)
				end
			end
			check attached num as n then
				Result := n
			end
		end

	power_of_ten_table: HASH_TABLE [JJ_BIG_NUMBER_64, JJ_BIG_NUMBER_64]
			-- Table used by `from_string' to memoize the powers of ten in the
			-- same representation as Current.  It contains a value indexed by
			-- a power.
			--     [ the value,  a power]
			-- It is deferred in {JJ_BIG_NATURAL} and defined in this class,
			-- because Eiffel does not allow a once function to have a generic
			-- or anchored result.
		once
			create Result.make (Default_table_size)
--			Result.compare_objects
		end

	limbs_needed_for_x_digits (a_count: INTEGER): INTEGER
			-- The number (i.e. `count') of bigits required to represent a
			-- base-10 number that hase `a_count' number of digits.
			-- http://stackoverflow.com/questions/12269096/how-many-bits-do-you-need-to-store-a-number
		local
			s: STRING_8
			i: INTEGER
			test: REAL
		do
			create s.make_filled ('9', a_count)
			i := s.to_integer
--			Result := log_base_2 (i) * bits_per_bigit
			test := log_2 (i)
			Result := (log_2 (i) / bits_per_bigit).truncated_to_integer + 1
		end

	log_base_2 (a_number: like Current): INTEGER
			-- The integral (i.e. floor) of the base-2 log of the `magnitude'
			-- of Current.  (Log of negative numbers gives a complex number.)
			-- http://stackoverflow.com/questions/12003719/log-of-a-very-large-number
		local
			i: INTEGER
			acc: INTEGER
			w: NATURAL_64
		do
				-- Find the most significant bigit, ignoring leading zeros.
			from i := a_number.count
			until i = 1 or else i_th (i) > zero_bigit
			loop
				i := i - 1
			end
				-- Get the high-order, non-zero bigit.
			w := a_number.i_th (i)
			from acc := 0
			until w <= zero_bigit
			loop
				acc := acc + 1
				w := w.bit_shift_right (acc)
			end
			Result := acc + a_number.count * bits_per_bigit - (bits_per_bigit + 1)
		end

	integer_as_bigit (a_integer: INTEGER_32): NATURAL_64
			-- The equivalent value of `a_integer' in same type as `bigit'
		require
			not_too_big: a_integer.abs <= max_bigit.as_integer_32
		do
			Result := a_integer.as_natural_32
		ensure
			same_values: Result.as_integer_32 ~ a_integer.abs
		end

	Default_table_size: INTEGER = 10
			-- The initial capacity assigned to the `power_of_ten_table'.

feature -- export for testing

	random: MELG_607
			-- Used to generate random numbers for placement into Current.
		once
			create Result
		end

invariant

	has_at_least_one_bigit: count >= 1

	is_zero_implies_non_negative: is_zero implies not is_negative
	is_negative_implies_non_zero: is_negative implies not is_zero

end
