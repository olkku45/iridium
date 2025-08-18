# Some docs

## Comptime

`comptime` evaluates either a block of code or a specific line of code at compile-time. 
For example:
```
fn main() {
    // single evaluation
    comptime const thing: f32 = 3.14;
	
	// block evaluation
	comptime {
	    const thing_two: f32 = 53434.91;
		const thing_three: f32 = 144269.1;
	}
}
```
If you have multiple variables back-to-back, that should be evaluated at compile-time, or 
a block of code, then the `comptime` block might be useful. 

## Arena allocation

`alloc_arena` is a memory allocator that lets you allocate a chunk of memory at once, that is also 
freed at once. In this language, memory allocated with an arena allocator is freed automatically 
at the end of a scope. Like with comptime, it can be used as a 'single', or a 'block', but there are differences, 
of course due to `alloc_arena` being a memory allocator. Let's see the two methods in action:
```
fn main() {
    // allocate an arena with the size 100 000 bytes to 'variable'
    const variable: []f32 = alloc_arena(100_000);
	
	// allocate an arena to a scope
	alloc_arena(1_000_000) {
	    const another_variable: []i32 = alloc(100_000);
	}
}
```
The allocation to `variable` is quite understandable. But with the scope allocation, you might not get what's 
happening, and that's okay! What's happening with scoped arena allocation is that the amount of memory 
allocated in `alloc_arena()` to the scope is called the 'budget' of the scope. Then in the scope itself, 
you can allocate memory normally. But how the memory allocation inside the block works, is that 
`alloc` draws the memory from the allocated arena, so `alloc` is figuratively taking a slice out of the memory pie. In the 
above example, the slice given to another_variable is a tenth of the allocated scoped memory arena pie.

If you exceed the memory budget in the scope, a runtime error will occur, unless you use `comptime`, which 
we'll get into later, and which would induce a compile-time error. 

If the arena of memory is unused, or some of it is unused, it just gets deallocated automatically, so no worries there.

## Scoped arena allocation and compile-time

Scoped arena allocation and compile-time evaluation have some effects on each other, as you may tell already. Let's go 
over it. `comptime` can be used with an `alloc_arena()`-block like this:
```
comptime alloc_arena(1_000) {
    const thing_one: i32 = alloc(100)
	const thing_two: i32 = alloc(200);
}
```
In this example, the block of code is evaluated at compile time. If we have a `comptime` keyword denoting the entire block, 
and another `comptime`keyword inside the block as well, the keyword inside the block would have no effect:
```
comptime alloc_arena(1_000) {
    comptime const thingy: []f32 = alloc(500);
}
```
Here comes the tricky part. Let's say we have this configuration:
```
alloc_arena(1_000) {
    comptime const a: i32 = alloc(400);
}
```
Here we have a `comptime` keyword only inside the block. The variable `a` gets evaluated at compile-time, and it doesn't 
pass the memory budget. But now, let's exceed the memory budget with another variable denoted with `comptime`:
```
alloc_arena(1_000) {
    comptime const a: i32 = alloc(400);
	comptime const b: i32 = alloc(700);
}
```
Here we get a compile-time error. The compiler lazily checks first if an alloc-call is inside an alloc_arena-block, and if it is, 
it will count the total memory taken from the given budget. Since both variables are denoted with `comptime`, the compiler checks 
both, sees that they exceed the allocated budget in total, and returns an error. But now, let's see another scenario:
```
alloc_arena(1_000) {
    comptime const a: i32 = alloc(400);
	const b: i32 = alloc(700);
}
```
Now `b` doesn't get evaluated at compile-time, rather only when this bit of code runs in the program. And you can probably tell that we 
exceed the allocated budget, but since the allocation that exceeds the budget isn't evaluated at compile-time, we don't get an error 
at compile-time, but only when the code has run up to this point! The point then is that you have to be careful with how these concepts 
interact, and take these things into consideration. An easy fix to this situation would be to just evaluate 
the whole block at compile-time to induce an error automatically, or if you want, you can denote both variables with `comptime` 
here. The ideal combination to use depends on your development scenario.