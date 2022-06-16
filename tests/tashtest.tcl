set case_number 0
set failed      0

proc cequal {s1 s2} {
   return [expr [string compare $s1 $s2] == 0]
}

proc test_case {command expected_result} {
   global case_number failed
   incr case_number
   catch {uplevel $command} actual_result
   if ![cequal $expected_result $actual_result] {
      incr failed
      puts stdout [format \
	 "Test Case %2d failed: %s: got '%s', expected: '%s'" \
	 $case_number $command $actual_result $expected_result]
   }
}


#| check argc and argv
test_case {set argc} 0
test_case {set argv} {}

#| test eq command
if {[info commands eq] == "eq"} {
   test_case {eq abc def} 0
   test_case {eq 1 1} 1
   set w1 .dlg
   set w2 .dlg.ok
   test_case {eq $w1.ok $w2} 1
}

#| test concat command
test_case {concat} {}
test_case {concat abc {def}} {abc def}
test_case {concat {a b c} d {e f} g h} {a b c d e f g h}

#| test list command
test_case {list} {}
test_case {list abc {x y} \}} {abc {x y} \}}

#| test object-oriented counter
test_case {counter} ctr0
test_case {counter} ctr1

ctr0 next; ctr0 next;
test_case {ctr0 get} { 2}
test_case {ctr1 get} { 0}
test_case {ctr0 clear} {bad counter command "clear": should be get or next}

rename ctr0 {};
test_case {ctr0 get} {invalid command name "ctr0"}

test_case {sum 2 3} { 5}
test_case {sum 011 0x14} { 31}
test_case {sum 8#011# 16#14#} { 29}
test_case {sum 2#011# 2#1_000#} { 11}
test_case {sum 3 6z} {expected integer but got "6z"}
test_case {sum a3 6} {expected integer but got "a3"}

#| simple expr command
set x 1
test_case {simple_expr $x+1} 2
test_case {simple_expr $x + 1} {wrong # args}
test_case {simple_expr} {wrong # args}

#| check if passed or failed
if $failed {
    puts stdout "Test FAILED"
    exit 1
} else {
    puts stdout "Test PASSED"
    exit 0
}

