// COMPILE-TEST
// EXTRA-FLAGS: -copy-prop -dump-ir -no-color

// Note that the ret uses the add result and save a load from @g by doing so

int g;

// CHECK: 	load	$0<i32>, [@g<i32>]
// CHECK: 	add	$1<i32>, $0<i32>, 1<u32>
// CHECK: 	store	[@g<i32>], $1<i32>
// CHECK: 	ret	$1<i32>
int test()
{
    ++g;
    return g;
}
