// COMPILE-TEST
// EXTRA-FLAGS: -cse -dump-ir -no-color

// Note that $0 (which is a) only loaded in once instead of everytime it was
// used, saving 3 loads

// CHECK: 	salloc	$0<i32*>
// CHECK: 	salloc	$2<i32*>
// CHECK: 	store	[$0<i32*>], $a
// CHECK: 	load	$3<i32>, [$0<i32*>]
// CHECK: 	store	[$2<i32*>], $3<i32>
// CHECK: 	mul	$7<i32>, $3<i32>, $3<i32>
// CHECK: 	add	$8<i32>, $3<i32>, $7<i32>
// CHECK: 	lsl	$10<i32>, $8<i32>, $3<i32>
// CHECK: 	ret	$10<i32>
int test(int a)
{
    int local_a = a;

    return local_a + local_a * local_a << local_a;
}
