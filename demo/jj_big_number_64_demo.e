note
	description: "[
		Class to demonstrate the {JJ_BIG_NUMBER} class.  It creates and uses
		a {JJ_BIG_NUMBER_TEST} object and lets that object print the
		demo values as it runs assert statements.
	]"
	author: "Jimmy J.Johnson"
	date: "1/12/26"

class JJ_BIG_NUMBER_64_DEMO

create
	make

feature {NONE} -- Initialization

	make
			-- Run the tests
		local
			i: INTEGER
		do
				-- Clearn console
			from i := 1
			until i >= 10
			loop
				io.new_line
				i := i + 1
			end
				-- Create the `tester' object
			create tester
			io.put_string ("  Begin Demo/Tester for JJ_BIG_NATURAL numbers: %N")

			run_all

			io.put_string ("end test %N")
		end

feature -- Constants

	test_count: INTEGER = 10
			-- Number of time to run each test

feature -- Access

	tester: JJ_BIG_NUMBER_64_TESTS
			-- The set of tests currently being executed

feature -- Basic operations

	run_all
			-- Demo/test all the features on the current `tester'.
		do
--			run_known_fails
			run_initialization_tests
			run_constants_tests
			run_access_tests
			run_status_report_tests
			run_query_tests
			run_simple_basic_operations_tests
			run_addition_and_subtraction_tests
			run_multiplication_tests
			run_exponetiation_tests
			run_division_tests
		end

	run_initialization_tests
			-- Test creation & related features of {JJ_BIG_NATURAL}
		do
			put_line ("Initialization")
			tester.default_create_test
			tester.set_with_integer
			tester.set_with_value
			tester.set_with_string
--			tester.set_with_array			-- give inconsistent results ???
			tester.set_random
			tester.set_random_with_digit_count
		end

	run_constants_tests
			-- Test the constants of {JJ_BIG_NATURAL}
		do
			put_line ("Constants")
			tester.bits_per_bigit
			tester.zero_bigit
			tester.one_bigit
			tester.two_bigit
			tester.three_bigit
			tester.four_bigit
			tester.five_bigit
			tester.six_bigit
			tester.seven_bigit
			tester.eight_bigit
			tester.nine_bigit
			tester.ten_bigit
			tester.sixteen_bigit
			tester.max_half_bigit
			tester.max_bigit
			tester.max_ten_power
			tester.default_karatsuba_threshold
			tester.default_div_limit
		end

	run_access_tests
			-- Test access features of {JJ_BIG_NATURAL}
		do
			put_line ("Access")
			tester.zero
			tester.one
			tester.ones
			tester.zeros
			tester.karatsuba_threshold
			tester.div_limit
			tester.hash_code
			tester.bit_count
		end

	run_status_report_tests
			-- Test access features of {JJ_BIG_NATURAL}
		do
			put_line ("Status report")
			tester.is_zero
			tester.is_one
			tester.is_even
			tester.is_base
--			tester.is_base_multiple
			tester.is_negative
			tester.divisible
		end

	run_query_tests
			-- Test the query features of {JJ_BIG_NATURAL}
		do
			put_line ("Query")
			tester.is_same_sign
			tester.is_less
			tester.is_magnitude_less
			tester.is_magnitude_equal
			tester.max
			tester.min
--			tester.magnitude_max
--			tester.magnitude_min
		end

	run_simple_basic_operations_tests
			-- Test simple basic operations features of {JJ_BIG_NATURAL}
		do
			put_line ("Basic Operations (simple)")
--			wipe_out
			tester.negate
			tester.increment
			tester.decrement
			tester.identity
			tester.opposite
			tester.magnitude
		end

	run_addition_and_subtraction_tests
			-- Test addition and subtraction features of {JJ_BIG_NATURAL}
		do
			put_line ("Basic Operations (addition & subtraction)")
			tester.scalar_sum
			tester.scalar_difference
			tester.plus
			tester.minus
		end

	run_multiplication_tests
			-- Test multiplication feature of {JJ_BIG_NATURAL}
		do
			put_line ("Basic Operations (multiplication)")
--			tester.scalar_multiply
			tester.scalar_product
--			tester.multiply
			tester.product
		end

	run_exponetiation_tests
			-- Test exponetiation feature of {JJ_BIG_NATURAL}
		do
			put_line ("Basic Operations (exponetiation)")
--			tester.raise
			tester.power
--			tester.power_modulo
		end

	run_division_tests
			-- Test the division features of {JJ_BIG_NATURAL}
		do
			put_line ("Basic Operations (division)")
			tester.quotient
			tester.integer_quotient
			tester.integer_remainder
		end

feature {NONE} -- Implementation

	put_line (a_string: STRING)
			-- Print a dividing line containing `a_string'
			-- (e.g.  "----------- a_string ------------"
		local
			w, c, n, i: INTEGER_32
		do
			io.put_string ("%N")
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


end
