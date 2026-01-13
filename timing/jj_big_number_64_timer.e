note
	description: "[
		Class to demonstrate the {JJ_BIG_NUMBER_64} class.  
		Times the product function of against GMP_INTEGER.
		The results are placed in files which can be plotted
		using GNUPlot.
	]"
	author: "Jimmy J.Johnson"
	date: "1/12/26"
	revision: "$Revision$"

class JJ_BIG_NUMBER_64_TIMER

create
	make

feature {NONE} -- Initialization

	make
			-- Run the tests
		local
			i: INTEGER
		do
			create jj_timer
			create gmp_timer
				-- Clearn console
			from i := 1
			until i >= 20
			loop
				io.new_line
				i := i + 1
			end
				-- Test implementation features.
			io.put_string ("------------- Running timing tests ----------------- %N")

			{MEMORY}.collection_off
			run_tests
			{MEMORY}.collection_on

--			time_multiplication
--			time_division

			io.put_string ("end test %N")
		end

feature -- Basic operations

	run_tests
			-- Run all tests.
		local
			s, type_s: STRING_8
			f, gmp_f: PLAIN_TEXT_FILE
			n: INTEGER
			arr: like time_it
			i, j: INTEGER
		do
			type_s := (create {like anchor}).generating_type
			from n := 1 until n > Max_values.count loop
				s := "{" + type_s + "}-product - max = " + max_values[n].out
				create f.make_open_write (s)
				s := "{GMP_INTEGER}-product - max = " + max_values[n].out
				create gmp_f.make_open_write (s)
				arr := time_it (agent {like anchor}.product, agent {GMP_INTEGER}.product, Max_values[n])
				from i := 1 until i > arr.width loop
					from j := 1 until j > arr.height loop
							-- Write jj data.
						s := fd.formatted (arr.item (i,j).jjj)
						f.put_string (s)
							-- Write gmp data.
						s :=fd.formatted (arr.item (i, j).gmp)
						gmp_f.put_string (s)
						j := j + 1
					end
					f.put_string ("%N")
					gmp_f.put_string ("%N")
					i := i + 1
				end
					-- Show resulting time arrays on screen
				from i := 1
				until i > arr.width
				loop
					from j := 1
					until j > arr.height
					loop
						io.put_string ("[" + fd.formatted (arr.item (i, j).gmp) + ", " +
											 fd.formatted (arr.item (i, j).jjj) + "]   ")
						j := j + 1
					end
					io.put_string ("%N")
					i := i + 1
				end
				io.new_line
				f.close
				n := n + 1
			end
		end

feature {NONE} -- Implementation

	fd: FORMAT_DOUBLE
			-- For formatting floating point values.
		once
			create Result.make (5, 2)
		end

	jj_timer: HMS_TIMER
			-- For timing execution times for {JJ_BIG_NATURAL} functions.

	gmp_timer: HMS_TIMER
			-- For timing execution times for gmp functions.

feature {NONE} -- Basic operations

feature {NONE} -- Implementation

	time_it (jj_function: FUNCTION [like anchor, like anchor];
				gmp_function: FUNCTION [like gmp_anchor, like gmp_anchor];
				a_max: INTEGER): ARRAY2 [TUPLE [jjj, gmp: REAL_64]]
			-- Run `a_function' `batch_size' times on numbers that grow
			-- `a_step' amount on each loop.  Place results of each call
			-- in Result.
		local
			n, inc: INTEGER
			b: INTEGER
			i, j: INTEGER
			jj_avg, gmp_avg: REAL_64
			tup_1, tup_2: like make_one_number
			v: like anchor
			gmp_v: like gmp_anchor
			v_out, gmp_out: STRING
		do
			inc := a_max // steps
			n := a_max // inc
			io.put_string ("Time_it:  n = " + n.out + "%N")
			create Result.make_filled ([0.0, 0.0], n, n)
			from i := n
			until i <= 0
			loop
				from j := n
				until j <= 0
				loop
					io.put_string ("i = " + i.out + "     j = " + j.out + ": %N")
					jj_timer.reset
					gmp_timer.reset
					from b := 1
					until b > batch_size
					loop
						tup_1 := make_one_number (i * inc)
						tup_2 := make_one_number (j * inc)
						io.put_string ("%N")
						{MEMORY}.collection_on
						{MEMORY}.full_coalesce
						{MEMORY}.full_collect
						{MEMORY}.collection_off
							-- Time {JJ_BIG_NATURAL}
						jj_timer.run
						jj_function.call ([tup_1.jj_n, tup_2.jj_n])
						jj_timer.stop
						{MEMORY}.collection_on
						{MEMORY}.full_coalesce
						{MEMORY}.full_collect
						{MEMORY}.collection_off
							-- Time {GMP_INTEGER}
						gmp_timer.run
						gmp_function.call ([tup_1.gmp_n, tup_2.gmp_n])
						gmp_timer.stop
						{MEMORY}.collection_on
						{MEMORY}.full_coalesce
						{MEMORY}.full_collect
--						{MEMORY}.collection_off
							-- Now check the results
--						v := jj_function.item ([tup_1.jj_n, tup_2.jj_n])
--						gmp_v := gmp_function.item ([tup_1.gmp_n, tup_2.gmp_n])
--						v_out := v.out
--						gmp_out := gmp_v.out
--						if v_out /~ gmp_out then
--							io.put_string ("   ERROR:  result:  " + v_out + "     expected " + gmp_out + "%N")
--						end
						b := b + 1
					end
					jj_avg := jj_timer.cumulative.as_milliseconds / batch_size
					gmp_avg := gmp_timer.cumulative.as_milliseconds / batch_size
					Result.put ([jj_avg, gmp_avg], i, j)
--					io.put_string ("%N")
					j := j - 1
				end
--				io.put_string ("%N")
				i := i - 1
			end
			io.new_line
		end

	make_one_number (a_count: INTEGER): TUPLE [jj_n: like anchor; gmp_n: like gmp_anchor]
			-- Create a tuple containing a random number in the two representations
			-- where each number contains `a_count' number of decimal digits.
		require
			count_big_enough: a_count >= 1
		local
			s: STRING_8
			i: INTEGER
			jj_n: like anchor
			gmp_n: like gmp_anchor
		do
			create jj_n.make_random_with_digit_count (a_count)
			s := jj_n.out
			create gmp_n.make_string (s)
--			io.put_string (s + "%N")
			io.put_string ("     make_one_number:  digit_count = " + a_count.out +
							"  count = " + jj_n.count.out + "  bit_count = " + jj_n.bit_count.out)
			Result := [jj_n, gmp_n]
		end

	results: ARRAYED_LIST [TUPLE [jj_time, gmp_time: REAL_64]]
			-- Results of running a particular function, indexed by the
			-- number of digits in the arguments.
		once
			create Result.make (10)
		end

	batch_size: INTEGER = 10
			-- The number of samples of each digit of a particular length.

	max_values: ARRAY [INTEGER]
			-- The maximum number of words/digits during a test.
		once
			Result := <<1000, 2000>>
		end

	steps: INTEGER = 10
			-- Number of intervals to test going from one to `max_values'.


	anchor: JJ_BIG_NUMBER_64
			-- Anchor.  Do not call
		once
			check
				do_not_call: false then
			end
		end

	gmp_anchor: GMP_INTEGER
			-- Anchor for GMP function calls
		once
			check
				do_not_call: false then
			end
		end

invariant


end
