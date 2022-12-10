// COMPILE-TEST
// EXTRA-FLAGS: -dump-ir -no-color


// CHECK: func test ($a :i32) -> i32:
// CHECK: .entry_test:
// CHECK: 	salloc	$0<i32*>
// CHECK: 	store	[$0<i32*>], $a
// CHECK: 	load	$2<i32>, [$0<i32*>]
// CHECK: 	ret	$2<i32>
signed int test(int a) { return a; }
