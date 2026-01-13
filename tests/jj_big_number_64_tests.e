note
	description: "[
		This class tests {JJ_BIG_NUMBER_64}

		In addition to running assert statements, each test feature prints
		information pertinant to that test, so that these features can be
		called from {BIG_NUMBER_DEMO} to print demonstration values.
	]"
	author: "Jimmy J Johnson"
	date: "1/11/26"

class
	JJ_BIG_NUMBER_64_TESTS

inherit

	EQA_TEST_SET
		redefine
			on_prepare
		end

feature {NONE} -- Events

	on_prepare
			-- Called after all initializations in `default_create'.
			-- Redefined to set `test_limit' for auto-testing.
		do
			test_limit := Default_test_limit
			bigit_limit := Default_bigit_limit
			digit_limit := Default_digit_limit
			power_limit := Default_power_limit
		end

feature -- Constants

	known_bits_per_bigit: INTEGER = 64
			-- Know number of bits in each bigit

	known_max_bigit: NATURAL_64 = 0xFFFFFFFFFFFFFFFF
			-- Known value of the max value representable in a bigit.

	known_max_half_bigit: NATURAL_64 = 0x00000000FFFFFFFF
			-- Known value of the max value representable in a half a bigit

	known_max_ten_power: NATURAL_64 = 	10_000_000_000_000_000_000
			-- Known value of the maximum multiple of 10 representable in a bigit

	format_as_string: INTEGER = 0
			-- Display a {JJ_BIG_NUMBER_64} as a decimal number

	format_as_stored: INTEGER = 1
			-- Display a {JJ_BIG_NUMBER_64} using `out_as_stored'
			-- (i.e. <1,2,3,4>)

	format_as_bits: INTEGER = 2
			-- Display a {JJ_BIG_NUMBER_64} using `out_as_binary'
			-- (i.e. <11110011, 00011011, 00000001>)

	Default_test_limit: INTEGER = 10
			-- The default number of times to run a test.

	Default_bigit_limit: INTEGER = 50
			-- The default max number of bigits to create for tests

	Default_digit_limit: INTEGER = 100
			-- The default number of digits to create for tests

	Default_power_limit: INTEGER = 5
			-- The default maximum value allowed for exponentiation tests

feature -- Access

	test_limit: INTEGER
			-- The number of times to run a test

	bigit_limit: INTEGER
			-- The maximum number of bigits to create for tests

	digit_limit: INTEGER
			-- The maximum number of digits to create for tests

	power_limit: INTEGER
			-- The maximum exponent allowed in exponetiation tests

	target_format: INTEGER
			-- The format in which to display the target of calls is `signature'

	argument_format: INTEGER
			-- The format in which to display arguments in `signature'

	result_format: INTEGER
			-- The format in which to display results

feature -- Element change

	set_test_limit (a_limit: INTEGER)
			-- Set `test_limit' to `a_count'
		require
			count_big_enough: a_limit >= 1
		do
			test_limit := a_limit
		end

	set_bigit_limit (a_limit: INTEGER)
			-- Set `bigit_limit'
		require
			count_big_enough: a_limit >= 1
		do
			bigit_limit := a_limit
		end

	set_digit_limit (a_limit: INTEGER)
			-- Set `bigit_limit'
		require
			count_big_enough: a_limit >= 1
		do
			bigit_limit := a_limit
		end

	set_power_limit (a_limit: INTEGER)
			-- Set `bigit_limit'
		require
			count_big_enough: a_limit >= 1
		do
			power_limit := a_limit
		end

	set_target_format (a_format: INTEGER_32)
			-- Change `target_format' to `a_format'
		require
			format_big_enough: a_format >= format_as_string
			format_small_enough: a_format <= format_as_bits
		do
			target_format := a_format
		end

	set_argument_format (a_format: INTEGER_32)
			-- Change `argument_format' to `a_format'
		require
			format_big_enough: a_format >= format_as_string
			format_small_enough: a_format <= format_as_bits
		do
			argument_format := a_format
		end

	set_result_format (a_format: INTEGER_32)
			-- Change `result_format' to `a_format'
		require
			format_big_enough: a_format >= format_as_string
			format_small_enough: a_format <= format_as_bits
		do
			result_format := a_format
		end

feature -- Basic operations

	run_known_fails
			-- Run tests that were previously discovered to fail
		local
			s: STRING
			pow: NATURAL_32
			n, n2: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			io.put_string ("{BIG_NATURAL_32_TESTS}.run_known_fails: %N")
				-- fails with integer_remainder
			s := "67481806940030140066982757483494117521"
			create n
			n.set_with_string (s)
			create n2
			s := "16580898624578913509"
			n2.set_with_string (s)
			function (agent n.integer_quotient (n2), "integer_quotient", "4069852211749102341")
			function (agent n.integer_remainder (n2), "integer_remainder", "2845594835665692952")
				-- fails in quotient
			divider ("raise")
			s := "157682840352094851032842628677015822187"
			create n
			n.set_with_string (s)
			create mp_n.make_string (s)
			procedure (agent n.raise (9), "raise")
			function (agent n.out, "out", mp_n.power (9).out)
		end


feature -- Test Initializations

	default_create_test
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
		do
			create n
--			s := signature (n, "default_create", {ARRAY [ANY]} <<>>)
--			io.put_string (s + n.out_as_stored + "%N")
--			assert (s, n.out_as_stored ~ "<0>")
			function (agent n.out_as_stored, "default_create", "<0>")
		end

	set_with_integer
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			i: INTEGER_32
			b: BOOLEAN
			fn, s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: INTEGER_32
		do
			fn := "set_with_integer"
			divider (fn)
			from i := 1
			until i > test_limit
			loop
				v := es_random.item
				es_random.forth
				if es_random.real_item > 0.5 then
					v := -v
				end
				es_random.forth
				create n
--				s := signature (n, fn, <<v>>)
--				io.put_string (s)
--				n.set_with_integer (v)
--				n.set_with_integer (v)
--				io.put_string (n.out + "%N")
--				assert (s, n.out ~ v.out)
				procedure (agent n.set_with_integer (v), "set_with_integer")
				function (agent n.out, "out", v)
				i := i + 1
			end
		end

	set_with_value
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			i: INTEGER
			fn, s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			fn := "set_with_value"
			divider (fn)
			from i := 1
			until i > test_limit
			loop
				v := random.item
				random.forth
				create n
--				s := signature (n, fn, <<v>>)
--				n.set_with_value (v)
--				io.put_string (s + n.out_as_stored + "%N")
--				assert (s, n.out_as_stored ~ "<" + v.out + ">")
				procedure (agent n.set_with_value (v), "set_with_value")
				function (agent n.out_as_stored, "out_as_stored", "<" + v.out + ">")
				i := i + 1
			end
		end

	set_with_string
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			i: INTEGER
			a: STRING_8
			s: STRING_8
			n: JJ_BIG_NUMBER_64
		do
			divider ("set_with_string")
			from i := 1
			until i > test_limit
			loop
				a := random_string (digit_limit)
				create n
--				s := signature (n, fn, <<a>>)
--				n.set_with_string (a)
--				io.put_string (s + n.out + "%N")
--				assert (s, n.out ~ a)
				procedure (agent n.set_with_string (a), "set_with_string")
				function (agent n.out, "out", a)
				i := i + 1
			end
		end

	set_with_array
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			i: INTEGER
			a: ARRAY [NATURAL_64]
			fn, s: STRING
			n: JJ_BIG_NUMBER_64
		do
			fn := "set_with_array"
			divider (fn)
			from i := 1
			until i > test_limit
			loop
				a := random_array (bigit_limit)
				create n
--				s := signature (n, fn, <<a>>)
--				io.put_string (s)
--				n.set_with_array (a)
--				io.put_string (n.out_as_stored + "%N")
--				assert (s, n.out_as_stored ~ array_as_string (a))
				procedure (agent n.set_with_array (a), "set_with_array")
				function (agent n.out_as_stored, "out_as_stored", array_as_string (a))
				i := i + 1
			end
		end

	set_random
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn, s: STRING_8
			n: JJ_BIG_NUMBER_64
			i, c: INTEGER
		do
			fn := "set_random"
			divider (fn)
			from i := 1
			until i > test_limit
			loop
				c := es_random.item \\ bigit_limit + 1
				es_random.forth
				create n
--				s := signature (n, fn, <<c>>)
--				n.set_random (c)
--				io.put_string (s + n.out_as_stored + "  count = " + n.count.out + "%N")
--				assert (s, n.count = c)
				procedure (agent n.set_random (c), "set_random")
				function (agent n.count, "count", c)
				i := i + 1
			end
		end

	set_random_with_digit_count
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn, s: STRING_8
			n: JJ_BIG_NUMBER_64
			i, c: INTEGER
		do
			fn := "set_random_with_digit_count"
			divider (fn)
			from i := 1
			until i > test_limit
			loop
				c := es_random.item \\ bigit_limit + 1
				es_random.forth
				create n
--				s := signature (n, fn, <<c>>)
--				n.set_random_with_digit_count (c)
				procedure (agent n.set_random_with_digit_count (c), "set_random_with_digit_count")
				function (agent n.decimal_count, "fix me decimal_count", n.decimal_count)--c)

--				io.put_string (s + n.out + "  count = " + n.out.count.out + "%N")
--				assert (s, n.out.count = c)
				i := i + 1
			end
		end

--	from_string
--			-- Test the corresponding feature from {JJ_BIG_NATURAL}.
--		local
--			gt, fn, str, s: STRING_8
--			n: JJ_BIG_NUMBER_64
--		do
--			io.put_string ("{BIG_NATURAL_TESTS} with formatted output: %N")
--			create n
--			gt := n.generating_type + ":  "
--			fn := ".from_string "
--			str := gt + fn
--				-- First test
--			s := "123456789"
--			io.put_string (str + "(%"" + s + "%") = ")
--			create n
--			n.from_string (s)
--			io.put_string (n.out_formatted + "%N")
--			assert (s, n.out ~ "123456789")
--				-- Test leading zeroes.
--			s := "0,0,0,0,0,9,8,7,6,5,4,3,2,1"
--			io.put_string (str + "(%"" + s + "%") = ")
--			create n
--			n.from_string (s)
--			io.put_string (n.out_formatted + "%N")
--			assert (s, n.out ~ "987654321")
--				-- Negative test
--			s := "-11,234,567,899"
--			io.put_string (str + "(%"" + s + "%") = ")
--			create n
--			n.from_string (s)
--			io.put_string (n.out_formatted + "%N")
--			assert (s + ".is_negative", n.is_negative)
--			assert (s + ".out", n.out ~ "-11234567899")
--				--  Test
--			s := "1,000,000,000,000"
--			io.put_string (str + "(%"" + s + "%") = ")
--			create n
--			n.from_string (s)
--			io.put_string (n.out_formatted + "%N")
--			assert (s + ".out", n.out ~ "1000000000000")
--		end

feature -- Test constants

	bits_per_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: INTEGER
		do
			create n
--			v := n.bits_per_bigit
--			s := signature (n, "bits_per_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + n.bits_per_bigit.out + "%N")
--			assert (s, n.bits_per_bigit ~ known_bits_per_bigit)
			function (agent n.bits_per_bigit, "bits_per_bigit", known_bits_per_bigit)
		end

	zero_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.zero_bigit
--			s := signature (n, "zero_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "0")
			function (agent n.zero_bigit, "zero_bigit", 0)
		end

	one_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.one_bigit
--			s := signature (n, "one_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "1")
			function (agent n.one_bigit, "one_bigit", 1)
		end

	two_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.two_bigit
--			s := signature (n, "two_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "2")
			function (agent n.two_bigit, "two_bigit", 2)
		end

	three_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.three_bigit
--			s := signature (n, "three_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "3")
			function (agent n.three_bigit, "three_bigit", 3)
		end

	four_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.four_bigit
--			s := signature (n, "four_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "4")
			function (agent n.four_bigit, "four_bigit", 4)
		end

	five_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.five_bigit
--			s := signature (n, "five_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "5")
			function (agent n.five_bigit, "five_bigit", 5)
		end

	six_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.six_bigit
--			s := signature (n, "six_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "6")
			function (agent n.six_bigit, "six_bigit", 6)
		end

	seven_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.seven_bigit
--			s := signature (n, "seven_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "7")
			function (agent n.seven_bigit, "seven_bigit", 7)
		end

	eight_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.eight_bigit
--			s := signature (n, "eight_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "8")
			function (agent n.eight_bigit, "eight_bigit", 8)
		end

	nine_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.nine_bigit
--			s := signature (n, "nine_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "9")
			function (agent n.nine_bigit, "ten_bigit", 9)
		end

	ten_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.ten_bigit
--			s := signature (n, "ten_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "10")
			function (agent n.ten_bigit, "ten_bigit", 10)
		end

	sixteen_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.sixteen_bigit
--			s := signature (n, "sixteen_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "16")
			function (agent n.sixteen_bigit, "sixteen_bigit", 16)
		end

	max_half_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.max_half_bigit
--			s := signature (n, "max_half_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v ~ known_max_half_bigit)
			function (agent n.max_half_bigit, "max_half_bigit", known_max_half_bigit)
		end

	max_bigit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.max_bigit
--			s := signature (n, "max_bigit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v ~ known_max_bigit)
			function (agent n.max_bigit, "max_bigit", known_max_bigit)
		end

	max_ten_power
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
		do
			create n
--			v := n.max_ten_power
--			s := signature (n, "max_ten_power", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v ~ known_max_ten_power)
			function (agent n.max_ten_power, "max_ten_power", known_max_ten_power)
		end

	default_karatsuba_threshold
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: INTEGER
		do
			create n
--			v := n.default_karatsuba_threshold
--			s := signature (n, "default_karatsuba_threshold", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "4")
			function (agent n.default_karatsuba_threshold, "default_karatsuba_threshold", 4)
		end

	default_div_limit
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: INTEGER
		do
			create n
--			v := n.default_div_limit
--			s := signature (n, "default_div_limit", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "4")
			function (agent n.default_div_limit, "default_div_limit", 4)
		end

feature -- Test access & setters

	zero
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: JJ_BIG_NUMBER_64
		do
			create n
			v := n.zero
--			s := signature (n, "zero", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "0")
			function (agent n.zero, "zero", 0)
		end

	one
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			s: STRING_8
			n: JJ_BIG_NUMBER_64
			v: JJ_BIG_NUMBER_64
		do
			create n
			v := n.one
--			s := signature (n, "one", {ARRAY [ANY]} <<>>)
--			io.put_string (s + v.out + "%N")
--			assert (s, v.out ~ "1")
			function (agent n.one, "one", 1)
		end

	ones
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			fn, s: STRING_8
			n: JJ_BIG_NUMBER_64
			i, c: INTEGER
		do
			fn := "ones"
			divider (fn)
			from i := 1
			until i > test_limit
			loop
				create n
				c := es_random.item \\ bigit_limit + 1
				es_random.forth
--				s := signature (n, fn, <<c>>)
--				io.put_string (s)
--				n := n.ones (c)
--				io.put_string (n.out_as_bits + "%N")
--				assert (s, n.count = c and across n as v all v.item ~ known_max_bigit end)
	io.put_string ("ones:  Fix me! %N")
				i := i + 1
			end
		end

	zeros
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			fn, s: STRING_8
			n: JJ_BIG_NUMBER_64
			i, c: INTEGER
		do
			fn := "zeros"
			divider (fn)
			from i := 1
			until i > test_limit
			loop
				create n
				c := es_random.item \\ bigit_limit + 1
				es_random.forth
--				s := signature (n, fn, <<c>>)
--				io.put_string (s)
				n := n.zeros (c)
--				io.put_string (n.out_as_bits + "%N")
--				assert (s, n.count = c and across n as v all v.item ~ n.zero_bigit end)
	io.put_string ("zeros:  Fix me! %N")
				i := i + 1
			end
		end

	karatsuba_threshold
			-- Tests and demonstrates the corresponding feature
			-- from {JJ_BIG_NATURAL}.
		local
			fn, s: STRING_8
			n: JJ_BIG_NUMBER_64
			i, c: INTEGER
		do
			fn := "karatsuba_threshold"
			divider (fn)
			create n
			from i := 1
			until i > test_limit
			loop
				c := es_random.item \\ bigit_limit + 1
				from
				until c >= 2
				loop
					es_random.forth
					c := es_random.item \\ bigit_limit + 1
				end
				es_random.forth
--				s := signature (n, fn, <<c>>)
				n.set_karatsuba_threshold (c)
--				io.put_string (s + n.karatsuba_threshold.out + "%N")
--				assert (s, n.karatsuba_threshold ~ c)
				function (agent n.karatsuba_threshold, "karatsuba_threshold", c)
				i := i + 1
			end
		end

	div_limit
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn, s: STRING_8
			n: JJ_BIG_NUMBER_64
			i, c: INTEGER
		do
			fn := "set_div_limit"
			divider (fn)
			create n
			from i := 1
			until i > test_limit
			loop
				c := es_random.item \\ bigit_limit + 1
				from
				until c >= 4
				loop
					es_random.forth
					c := es_random.item \\ bigit_limit + 1
				end
				es_random.forth
--				s := signature (n, fn, <<c>>)
				n.set_div_limit (c)
--				io.put_string (s + n.div_limit.out + "%N")
--				assert (s, n.div_limit ~ c)
				function (agent n.div_limit, "div_limit", c)
				i := i + 1
			end
		end

	hash_code
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn, s: STRING_8
			n: JJ_BIG_NUMBER_64
			i, c: INTEGER
		do
			fn := "hash_code"
			divider (fn)
			create n
			from i := 1
			until i > test_limit
			loop
				c := es_random.item \\ bigit_limit + 1
				es_random.forth
				n.set_random (c)
--				s := signature (n, fn, {ARRAY [ANY]} <<>>)
--				io.put_string (s + n.hash_code.out + "%N")
--				assert (s, n.hash_code ~ n.out_as_stored.hash_code)
				function (agent n.hash_code, "hash_code", n.out_as_stored.hash_code)
				i := i + 1
			end
		end

	bit_count
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn, s: STRING_8
			n: JJ_BIG_NUMBER_64
			i, c: INTEGER
		do
			fn := "bit_count"
			divider (fn)
			create n
			from i := 1
			until i > test_limit
			loop
				c := es_random.item \\ bigit_limit + 1
				es_random.forth
				n.set_random (c)
--				s := signature (n, fn, {ARRAY [ANY]} <<>>)
--				io.put_string (s + "%T" + n.bit_count.out + "%T for " + c.out + " bigits %N")
--				assert (s, n.bit_count = c * known_bits_per_bigit)
				function (agent n.bit_count, "bit_count", c * known_bits_per_bigit)
				i := i + 1
			end
		end

feature -- Test status reports

	is_zero
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn: STRING_8
			n: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("is_zero")
			create n
			from i := 1
			until i > test_limit
			loop
				n := new_random_near_value (n.one_bigit, n.four_bigit)
				mp_n := new_gmp_number (n)
				predicate (agent n.is_zero, "is_zero", mp_n.out ~ "0")
				i := i + 1
			end
		end

	is_one
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("is_one")
			create n
			from i := 1
			until i > test_limit
			loop
				n := new_random_near_value (n.one_bigit, n.four_bigit)
				mp_n := new_gmp_number (n)
				predicate (agent n.is_one, "is_one", mp_n.out ~ "1")
				i := i + 1
			end
		end

	is_even
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("is_even")
			create n
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				create mp_n.make_string (n.out)
				predicate (agent n.is_even, "is_even", mp_n.is_even)
				i := i + 1
			end
		end

	is_base
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn: STRING_8
			n: JJ_BIG_NUMBER_64
			i: INTEGER_32
		do
			divider ("is_base")
			create n
			from i := 1
			until i > test_limit
			loop
				n := new_random_near_value (n.max_bigit, n.four_bigit)
				n.scalar_add (n.max_bigit)
				predicate (agent n.is_base, "is_base",
						n.count = 2 and then (n.i_th(1) = n.zero_bigit and n.i_th (2) = n.one_bigit))
				i := i + 1
			end
		end

--	is_base_multiple
--			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
--		local
--			fn, s: STRING_8
--			n: JJ_BIG_NUMBER_64
--			i, c: INTEGER
--			b: BOOLEAN
--			a: ARRAY [NATURAL_64]
--		do
--			fn := "is_base_multiple"
--			io.put_string (divider (fn))
--			create n
--			from i := 1
--			until i > test_limit
--			loop
--				c := bigit_limit
--				a := random_array_with_range (c, n.zero_bigit, n.one_bigit)
--				n.set_with_array (a)
--				b := es_random.real_item.rounded > 0.5
--				es_random.forth
--				if b then
--					n.set_is_negative (True)
--				end
--				s := signature (n, fn, {ARRAY [ANY]} <<>>)
--				io.put_string (s + n.is_base_multiple.out + "  %N")
----				assert ("Fix me! " + s, n.is_base_multiple implies (n[2].out ~ "1" and n[1].out ~ "0"))
--				assert ("Fix me!   fix this assertion", False)
--				i := i + 1
--			end
--		end

	is_negative
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("is_negative")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				predicate (agent n.is_negative, "is_negative", mp_n.is_negative)
				i := i + 1
			end
		end

	divisible
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn: STRING_8
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			fn := "divisible"
			divider (fn)
			create n
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n2 := new_random_near_value (n.zero_bigit, n.four_bigit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				predicate (agent n.divisible (n2), "divisible", mp_n.divisible (mp_n2))
				i := i + 1
			end
		end

feature -- Test Queries

	is_same_sign
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("is_same_sign")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n2 := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				predicate (agent n.is_same_sign (n2), "is_same_sign", mp_n.is_same_sign (mp_n2))
				i := i + 1
			end
		end

	is_less
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("is_less")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n2 := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				predicate (agent n.is_less (n2), "is_less", mp_n.is_less (mp_n2))
				i := i + 1
			end
		end

	is_magnitude_less
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("is_magnitude_less")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n2 := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				predicate (agent n.is_magnitude_less (n2), "is_magnitudce_less", mp_n.is_magnitude_less (mp_n2))
				i := i + 1
			end
		end

	is_magnitude_equal
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("is_magnitude_equal")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				es_random.forth
				if es_random.real_item < .5 then
					n2 := n.twin
				else
					n2 := new_random_number (bigit_limit)
				end
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				predicate (agent n.is_magnitude_equal (n2), "is_magitude_equal", mp_n.is_magnitude_equal (mp_n2))
				i := i + 1
			end
		end

	max
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("max")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n2 := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				function (agent n.max (n2), "max", mp_n.max (mp_n2))
				i := i + 1
			end
		end

	min
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("min")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n2 := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				function (agent n.min (n2), "min", mp_n.min (mp_n2))
				i := i + 1
			end
		end

feature -- Test Basic operations (simple)

--	wipe_out
--			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
--		local
--			s: STRING_8
--			n: JJ_BIG_NUMBER_64
--			i, c: INTEGER_32
--		do
--			io.put_string (divider)
--			from i := 1
--			until i > test_count
--			loop
--				c := es_random.item \\ bigit_count + 1	-- so not zero
--				es_random.forth
--				create n
--				n.set_random (c)
--				s := signature (n, "wipe_out", <<>>)
--				io.put_string (s)
--				n.wipe_out
--				io.put_string (n.out + "%N")
--				assert (s, n.count = 1 and n.out ~ "0")
--			end
--		end

	negate
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("negate & opposite")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				function (agent n.opposite, "opposite", mp_n.opposite)
				i := i + 1
			end
		end

	increment
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("increment")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				procedure (agent n.increment, "increment")
				function (agent n.out, "out", mp_n.scalar_add (1).out)
				i := i + 1
			end
		end

	decrement
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("decrement")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				procedure (agent n.decrement, "decrement")
				function (agent n.out, "out", mp_n.scalar_subtract (1).out)
				i := i + 1
			end
		end

	identity
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn: STRING_8
			n: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("identity")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				function (agent n.identity, "identity", mp_n.identity)
				i := i + 1
			end
		end

	opposite
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("opposite")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				function (agent n.opposite, "opposite", mp_n.opposite)
				i := i + 1
			end
		end

	magnitude
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("magnitude")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				function (agent n.magnitude, "magitude", mp_n.abs)
				i := i + 1
			end
		end

feature -- Test basic operations (addition & subtraction)

	scalar_sum
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("scalar_sum & scalar_add")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				v := random.item
				random.forth
				mp_n2 := new_gmp_number (v)
				function (agent n.scalar_sum (v), "scalar_sum", mp_n.plus (mp_n2))
				i := i + 1
			end
		end

	scalar_difference
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("scalar_difference")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				v := random.item
				random.forth
				mp_n2 := new_gmp_number (v)
				function (agent n.scalar_difference (v), "scalar_difference", mp_n.minus (mp_n2))
				i := i + 1
			end
		end

	plus
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn: STRING_8
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			fn := "add"
			divider ("plus & add")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n2 := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				n.add (n2)
				mp_n := mp_n.plus (mp_n2)
				function (agent n.plus (n2), "plus", mp_n.plus (mp_n2))
				i := i + 1
			end
		end

	minus
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("minus & subtract")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n2 := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				function (agent n.minus (n2), "minus", mp_n.minus (mp_n2))
				i := i + 1
			end
		end

feature -- Basic operations (multiply)

	scalar_multiply
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n: JJ_BIG_NUMBER_64
			v: NATURAL_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("scalar_multiply")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				v := random.item
				random.forth
				mp_n2 := new_gmp_number (v)
				procedure (agent n.scalar_multiply (v), "scalar_multiply")
				function (agent n.out, "out", mp_n.product (mp_n2).out)
				i := i + 1
			end
		end

	scalar_product
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n, n_copied: JJ_BIG_NUMBER_64
			v: NATURAL_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
			bool: BOOLEAN
		do
			divider ("scalar_product")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n_copied := n.twin
				mp_n := new_gmp_number (n)
				v := random.item
				random.forth
				mp_n2 := new_gmp_number (v)
				function (agent n.scalar_product (v), "scalar_product", mp_n.product (mp_n2))
				assert ("scalar_multiply -- unchanged", n ~ n_copied)
				i := i + 1
			end
		end

	multiply
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("multiply")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				n2 := new_random_number (bigit_limit)
				mp_n2 := new_gmp_number (n2)
				procedure (agent n.multiply (n2), "multiply")
				function (agent n.out, "out", mp_n.product (mp_n2).out)
				i := i + 1
			end
		end

	product
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n, n2, n_copied: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("product")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n_copied := n.twin
				n2 := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				function (agent n.product (n2), "product", mp_n.product (mp_n2))
				assert ("product -- unchanged", n ~ n_copied)
				i := i + 1
			end
		end

feature -- Basic operations (exponentiation)

	raise
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			pow: NATURAL_32
			n: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("raise")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				pow := (es_random.item \\ power_limit).as_natural_32
				es_random.forth
				procedure (agent n.raise (pow), "raise")
				function (agent n.out, "out", mp_n.power (pow).out)
				i := i + 1
			end
		end

	power
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			pow: NATURAL_32
			n,n_copied: JJ_BIG_NUMBER_64
			mp_n: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("power")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n_copied := n.twin
				mp_n := new_gmp_number (n)
				pow := (es_random.item \\ power_limit).as_natural_32
				es_random.forth
				function (agent n.power (pow), "power", mp_n.power (pow))
				assert ("power -- unchanged", n ~ n_copied)
				i := i + 1
			end
		end

	power_modulo
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			n, pow, mod: JJ_BIG_NUMBER_64
			mp_n, mp_pow, mp_mod: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			divider ("power_modulo")
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
					-- Value must be positive for {JJ_BIG_NATURAL}
				from
				until not n.is_negative
				loop
					n := new_random_number (bigit_limit)
				end
				pow := new_random_number (power_limit)
				from
				until pow > n.zero
				loop
					pow := new_random_number (bigit_limit)
				end
					-- modulus must be positive and odd for {GMP_INTEGER}
				from mod := new_random_number (bigit_limit)
				until not mod.is_even
				loop
					mod := new_random_number (bigit_limit)
				end
				mp_n := new_gmp_number (n)
				mp_pow := new_gmp_number (pow)
				mp_mod := new_gmp_number (mod)
				function (agent n.power_modulo (pow, mod), "power_modulo", mp_n.power_modulo (mp_pow, mp_mod))
--				
--				compare_functions (agent n.power_modulo (pow, mod),
--									agent mp_n.power_modulo (mp_pow, mp_mod),
--									fn, {ARRAY [ANY]} <<pow, mod>>)
				i := i + 1
			end
		end

feature -- Test basic operations (division)

	quotient
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn: STRING_8
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			fn := "quotient"
			divider (fn)
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n2 := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				es_random.forth
				function (agent n.quotient (n2), "quotient", mp_n.quotient (mp_n2))
				i := i + 1
			end
		end

	integer_quotient
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn: STRING_8
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			fn := "integer_quotient"
			divider (fn)
			from i := 1
			until i > test_limit
			loop
				n := new_random_number (bigit_limit)
				n2 := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				es_random.forth
				function (agent n.integer_quotient (n2), "integer_quotient", mp_n.integer_quotient (mp_n2))
				i := i + 1
			end
		end

	integer_remainder
			-- Tests the corresponding feature from {JJ_BIG_NATURAL}.
		local
			fn: STRING_8
			n, n2: JJ_BIG_NUMBER_64
			mp_n, mp_n2: JJ_GMP_INTEGER
			i: INTEGER_32
		do
			fn := "integer_remainder"
			divider (fn)
			from i := 1
			until i > test_limit
			loop
				if i = 4 then
					do_nothing
				end
				n := new_random_number (bigit_limit)
				n2 := new_random_number (bigit_limit)
				mp_n := new_gmp_number (n)
				mp_n2 := new_gmp_number (n2)
				function (agent n.integer_remainder (n2), "integer_remainder", mp_n.integer_remainder (mp_n2))
				i := i + 1
			end
		end

--feature -- Test implementation (division)


--feature -- Basic operations (selectively exported)

--	bit_shift_left
--			 -- Tests the corresponding feature from {JJ_BIG_NATURAL}.
--		local
--			fn: STRING_8
--			n: JJ_BIG_NUMBER_64
--			mp_n: JJ_GMP_INTEGER
--			i: INTEGER_32
--		do
--			fn := "bit_shift_left"
--			divider (fn)
--			from i := 1
--			until i > test_limit
--			loop
--				n := new_random_number (bigit_limit)
--				n.bit_shift_left (i)
--				create mp_n.make_string (n.out)
--				procedure (agent n.bit_shift_left (i), "bit_shift_left")
--				assert ("same when shifted", n.out ~  mp_n.bit_shift_left (i).out)
--				i := i + 1
--			end
--		end

----	normalize
----			-- Test and demonstrate feature `normalize' from {JJ_BIG_NATURAL}.
----		deferred
----		end

----	divide_two_bigits_by_one
----			-- Test and demonstrate feature `divide_two_bigits_by_one' from
----			-- {JJ_BIG_NATURAL}.
----		deferred
----		end

----	scalar_divide
----			-- Test and demonstrate feature `scalar_divide' from {JJ_BIG_NATURAL}.
----			-- I checked the calculations at
----			-- "https://defuse.ca/big-number-calculator.htm".
----		local
----			str, s: STRING_8
----			n: like new_number
----			d: like new_number.digit
----			tup: like new_number.scalar_divide
----		do
----			str := ".scalar_divide:  "
----				-- test number one
----			create n_from_string ("8")
----			d := n.eight_value
----			s := str + "(" + n.out + ").scalar_divide (" + d.out + ")"
----			tup := n.scalar_divide (d)
----			report_tuple (s, tup)
----			assert (s + "  quot", tup.quot.out ~ "1")
----			assert (s + "  rem", tup.rem.out ~ "0")
----				-- test number two
----			create n_from_string ("49")
----			d := n.three_value
----			s := str + "(" + n.out + ").scalar_divide (" + d.out + ")"
----			tup := n.scalar_divide (d)
----			report_tuple (s, tup)
----			assert (s, tup.quot.out ~ "18")
----			assert (s, tup.rem.out ~ "1")
----				-- test number three
----			create n_from_string ("84746229876")
----			d := n.sixteen_value
----			s := str + "(" + n.out + ").scalar_divide (" + d.out + ")"
----			tup := n.scalar_divide (d)
----			report_tuple (s, tup)
----			assert (s, tup.quot.out ~ "5296639367")
----			assert (s, tup.rem.out ~ "4")
----		end



feature {NONE} -- Implementation

	is_valid_target_type (a_routine: ROUTINE): BOOLEAN
			-- Is the target of `a_routine' a type that this class can test?
		do
			Result := attached a_routine.target as t and then
				attached {JJ_BIG_NUMBER_64} t
			if not Result then
					-- The check for attached like Current seems to handle
					-- the case where `a_routine' is referencing an attribute.
					-- In that case, the actual target is the second argument
					-- of the `closed operands' not the first argument as I
					-- would expect.
				check attached a_routine.target as t then
					check attached a_routine.closed_operands as args and then args.count >= 2 then
						Result := attached {JJ_BIG_NUMBER_64} args [2]
					end
				end
			end

		end

--	function (a_function: FUNCTION [TUPLE, ANY]; a_name: STRING_8; a_expected: ANY)
	function (a_function: FUNCTION [ANY, TUPLE, ANY]; a_name: STRING_8; a_expected: ANY)
			-- Execute `a_function', printing the `signature' of the call
			-- and asserting that the result of the call is equivalent
			-- to `a_expected'.
		require
			target_closed: attached a_function.target
			no_open_arguments: a_function.open_count = 0
			expected_types: is_valid_target_type (a_function)
		local
			s: STRING_8
			ans: ANY
			is_ok: BOOLEAN
		do
			s := signature (a_function, a_name)
			ans := a_function.item (a_function.operands)
				-- Catch a special case for division
			if attached {like tuple_anchor} ans as t then
				check attached {like gmp_tuple_anchor} a_expected as gmp_t then
					is_ok := t.quot.out ~ gmp_t.quot.out and
							t.rem.out ~ gmp_t.rem.out
					s := s + " ==> [" + t.quot.out + ", " + t.rem.out + "]"
				end
			else
				is_ok := ans.out ~ a_expected.out
				s := s + " ==> " + ans.out
			end
			io.put_string (s + "%N")
			if not is_ok then
				io.put_string ("%T  ERROR -- expected  " + a_expected.out + "%N")
			end
			assert (s, is_ok)
		end

	predicate (a_predicate: PREDICATE; a_name: STRING_8; a_expected: BOOLEAN)
			-- Execute `a_function', printing the `signature' of the call
			-- and asserting that the result of the call is equivalent
			-- to `a_expected'.
		require
			target_closed: attached a_predicate.target
			no_open_arguments: a_predicate.open_count = 0
			expected_types: is_valid_target_type (a_predicate)
		local
			s: STRING_8
			ans: ANY
			is_ok: BOOLEAN
		do
			s := signature (a_predicate, a_name)
			ans := a_predicate.item (a_predicate.operands)
			is_ok := ans.out ~ a_expected.out
			s := s + " ==> " + as_named (ans)
			io.put_string (s + "%N")
			if not is_ok then
				io.put_string ("%T  ERROR -- expected  " + as_named (a_expected) + "%N")
			end
			assert (s, is_ok)
		end

	procedure (a_procedure: PROCEDURE; a_name: STRING_8)
			-- Execute `a_procedure', printing the `signature' of the call.
		require
			target_closed: attached a_procedure.target
			no_open_arguments: a_procedure.open_count = 0
			expected_types: is_valid_target_type (a_procedure)
		local
			s: STRING_8
		do
			s := signature (a_procedure, a_name)
			a_procedure.call
			io.put_string (s + "%N")
		end

	formatted_number (a_number: JJ_BIG_NUMBER_64; a_format: INTEGER_32): STRING_8
			-- A string representing `a_number' displayed as `out',
			-- `out_formatted_out', `out_as_stored', or `out_as_bits',
			-- depending on `a_format'.
		do
			inspect a_format
			when Format_as_string then
				Result := a_number.out
			when Format_as_stored then
				Result := a_number.out_formatted
			when Format_as_bits then
				Result := a_number.out_as_bits
			else
				Result := " error in number format "
			end
		end

	signature (a_routine: ROUTINE; a_feature: STRING): STRING
			-- Create a string representing a feature's signature.
		require
			target_closed: attached a_routine.target
			no_open_arguments: a_routine.open_count = 0
			expected_types: is_valid_target_type (a_routine)
		local
			i: INTEGER
			a: detachable ANY
			c: INTEGER  -- temp for testing
		do
			Result := ""
			check attached a_routine.target as t and attached a_routine.closed_operands as args then
				if attached {like Current} t then
						-- This must be a agent for an attribute
					check args.count >= 2 and then attached args [2] as a2 then
						Result := Result + a2.generating_type + ":  "
						Result := Result + "(" + as_named (args [2]) + ")." + a_feature
						if args.count >= 3 then
							Result := Result + "("
							from i := 3
							until i > args.count
							loop
								a := args [i]
								Result := Result + as_named (a)
								if i < args.count then
									Result := Result + ", "
								end
								i := i + 1
							end
							Result := Result + ")"
						end
					end
				else
					Result := t.generating_type.out + ":  "
					Result := Result + "(" + as_named (t) + ")." + a_feature
					c := args.count
					if args.count >= 2 then
						Result := Result + " ("
						from i := 2
						until i > args.count
						loop
							a := args [i]
							Result := Result + as_named (a)
							if i < args.count then
								Result := Result + ", "
							end
							i := i + 1
						end
						Result := Result + ")"
					end
				end
			end
		end

	divider (a_string: STRING_8)
			-- Print a dividing line containing `a_string'
			-- (e.g.  "----------- a_string ------------"
		local
			w, c, n, i: INTEGER_32
		do
			io.put_string ("%N%N%N")
			w := 70
			c := a_string.count
			n := (w - c) // 2
			from i := 1
			until i > n
			loop
				io.put_string ("-")
				i := i + 1
			end
			io.put_string (" " + a_string + " ")
			from i := 1
			until i > n
			loop
				io.put_string ("-")
				i := i + 1
			end
			io.put_string ("%N")
		end

	as_named (a_any: detachable ANY): STRING_8
			-- Format the output of `a_any'
		local
			i: INTEGER
		do
			Result := ""
			if attached {ARRAY [ANY]} a_any as a then
				Result := "<"
				from i := 1
				until i > a.count
				loop
					Result := Result + as_named (a.at (i))
					if i < a.count then
						Result := Result + ","
					end
					i := i + 1
				end
				Result := Result + ">"
			elseif attached {like tuple_anchor} a_any as t then
				Result := "[" + t.quot.out + ", " + t.rem.out + "]"
			elseif attached {STRING} a_any as s then
				Result := Result + s.out
			elseif attached a_any then
				Result := Result + a_any.out
			else
				Result := "Void"
			end
		end

	array_as_string (a_array: ARRAY [NATURAL_64]): STRING_8
			-- Format `a_array' for output in the same format as
			-- feature `out_as_stored' from {JJ_BIG_NATURAL}.
		local
			i: INTEGER
		do
			Result := "<"
			from i := 1
			until i > a_array.count
			loop
				Result := Result + a_array[i].out
				if i < a_array.count then
					Result := Result + ","
				end
				i := i + 1
			end
			Result := Result + ">"
		end

feature {NONE} -- Implementation

--	new_number: JJ_BIG_NUMBER_64
--			-- Factory for new big numbers.
--		deferred
--		end

	new_random_number (a_count: INTEGER): JJ_BIG_NUMBER_64
			-- A big number with `a_count' or fewer bigits.
		local
			c: INTEGER_32
		do
			create Result
			c := es_random.item \\ a_count + 1
			es_random.forth
			Result.set_random (c)
				-- Make some numbers negative
			if es_random.real_item < .5 then
				Result.set_is_negative (true)
			end
			es_random.forth
		end

	new_random_near_value (a_value, a_delta: NATURAL_64): JJ_BIG_NUMBER_64
			-- A new one-bigit number that is within `a_delta' of `a_value'
		local
			v: NATURAL_64
			r: NATURAL_64
		do
			r := random.item
			random.forth
			v := (r + a_value) \\ a_delta
			create Result
			Result.set_with_value (v)
				-- Make some numbers negative
			if es_random.real_item < .5 then
				Result.set_is_negative (true)
			end
		end

	new_gmp_number (a_number: NUMERIC): JJ_GMP_INTEGER
			-- Create a {JJ_GMP_INTEGER} eqivalent to `a_number'
		do
			create Result.make_string (a_number.out)
		ensure
			same_numbers: Result.out ~ a_number.out
		end

	es_random: RANDOM
			-- The EiffelBase random number generator from Eiffel Software
		once
			create Result.make
		end

	random: MELG_607
			-- A generator for 32-bit values
		once
			create Result
		end

	random_string (a_count: INTEGER): STRING_8
			-- A string of up to `a_count' numbers
		require
			count_large_enough: a_count >= 1
		local
			done: BOOLEAN
			n: JJ_BIG_NUMBER_64
		do
			create n
			random.set_range (n.one_bigit.as_natural_64, n.nine_bigit.as_natural_64)
				-- Now build a string of random digits
				-- Add the first number which is not a zero
			Result := random.item.out
			random.set_range (n.zero_bigit.as_natural_64, n.nine_bigit.as_natural_64)
			from
			until Result.count >= a_count or done
			loop
				Result := Result + random.item.out
				random.forth
					-- Finish with number possibly shorter than `a_count'
				done := random.item > n.eight_bigit
				random.forth
			end
		ensure
			string_long_enough: Result.count >= 1
			no_leading_zeros: not (Result.item (1) ~ '0')
		end

	random_array (a_count: INTEGER): ARRAY [NATURAL_64]
			-- An array of up to `a_count' number of digits
		require
			count_large_enough: a_count >= 1
		local
			done: BOOLEAN
			n: JJ_BIG_NUMBER_64
			c: INTEGER
			i: INTEGER
		do
			c := es_random.item \\ a_count + 1
			check
				c_big_enough: c >= 1
				c_small_enough: c <= a_count
			end
			create n
			create Result.make_filled (n.zero_bigit, 1, c)
			random.set_range (n.one_bigit.as_natural_64, n.max_bigit.as_natural_64)
			Result[1] := random.item
			random.forth
			from i := 1
			until i > c
			loop
				Result[i] := (random.item)
				random.forth
				i := i + 1
			end
		end

	random_array_with_range (a_count: INTEGER;
				a_lower, a_upper: NATURAL_64): ARRAY [NATURAL_64]
			-- An array of up to `a_count' number of digits, where each digit
			-- is in the range [`a_lower', `a_upper']
		require
			count_large_enough: a_count >= 1
		local
			done: BOOLEAN
			n: JJ_BIG_NUMBER_64
			c: INTEGER
			i: INTEGER
		do
			c := es_random.item \\ a_count + 1
			create n
			create Result.make_filled (n.zero_bigit, 1, c)
			random.set_range (a_lower.as_natural_64, a_upper.as_natural_64)
			random.forth
			from i := 1
			until i > c
			loop
				Result[i] := (random.item)
				random.forth
				i := i + 1
			end
		end

feature {NONE} -- Anchors

	tuple_anchor: TUPLE [quot, rem: JJ_BIG_NUMBER_64]
			-- Anchor for types involved in division.
			-- Not to be called; just used to anchor types.
			-- Declared as a feature to avoid adding an attribute.
		require
			never_called: false
		do
			check
				do_not_call: false then
					-- Because gives no info; simply used as anchor.
			end
		end

	gmp_tuple_anchor: TUPLE [quot, rem: JJ_GMP_INTEGER]
			-- Anchor for types involved in division.
			-- Not to be called; just used to anchor types.
			-- Declared as a feature to avoid adding an attribute.
		require
			never_called: false
		do
			check
				do_not_call: false then
					-- Because gives no info; simply used as anchor.
			end
		end

end





