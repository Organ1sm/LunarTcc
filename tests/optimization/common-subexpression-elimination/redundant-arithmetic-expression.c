// COMPILE-TEST
// EXTRA-FLAGS: -cse -dump-ir -no-color

// Note that only one "add" instruction present in the output instead of 3

// CHECK: 	salloc	$0<i32*>
// CHECK: 	salloc	$2<i32*>
// CHECK: 	store	[$0<i32*>], $a
// CHECK: 	store	[$2<i32*>], $b
// CHECK: 	load	$4<i32>, [$0<i32*>]
// CHECK: 	load	$5<i32>, [$2<i32*>]
// CHECK: 	add	$6<i32>, $4<i32>, $5<i32>
// CHECK: 	mul	$10<i32>, $6<i32>, $6<i32>
// CHECK: 	sub	$14<i32>, $10<i32>, $6<i32>
// CHECK: 	ret	$14<i32>
int test(int a, int b) { return (a + b) * (a + b) - (a + b); }
