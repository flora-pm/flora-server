declare external ccc i8* @malloc(i32)

declare external ccc void @free(i8*)

declare external ccc void @llvm.memset.p0i8.i64(i8*, i8, i64, i1)

declare external ccc void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i1)

declare external ccc i32 @memcmp(i8*, i8*, i64)

%node_data_t_0 = type {%node_t_0*, i16, i16, i1}

%node_t_0 = type {%node_data_t_0, [20 x [3 x i32]]}

%inner_node_t_0 = type {%node_t_0, [21 x %node_t_0*]}

%btree_iterator_t_0 = type {%node_t_0*, i16}

%btree_t_0 = type {%node_t_0*, %node_t_0*}

define external ccc i8 @eclair_btree_value_compare_0(i32 %lhs_0, i32 %rhs_0) {
start:
  %0 = icmp ult i32 %lhs_0, %rhs_0
  br i1 %0, label %if_0, label %end_if_0
if_0:
  ret i8 -1
end_if_0:
  %1 = icmp ugt i32 %lhs_0, %rhs_0
  %2 = select i1 %1, i8 1, i8 0
  ret i8 %2
}

define external ccc i8 @eclair_btree_value_compare_values_0([3 x i32]* %lhs_0, [3 x i32]* %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [3 x i32], [3 x i32]* %lhs_0, i32 0, i32 0
  %1 = getelementptr [3 x i32], [3 x i32]* %rhs_0, i32 0, i32 0
  %2 = load i32, i32* %0
  %3 = load i32, i32* %1
  %4 = call ccc i8 @eclair_btree_value_compare_0(i32 %2, i32 %3)
  %5 = icmp eq i8 %4, 0
  br i1 %5, label %comparison_2, label %end_0
comparison_1:
  %6 = getelementptr [3 x i32], [3 x i32]* %lhs_0, i32 0, i32 1
  %7 = getelementptr [3 x i32], [3 x i32]* %rhs_0, i32 0, i32 1
  %8 = load i32, i32* %6
  %9 = load i32, i32* %7
  %10 = call ccc i8 @eclair_btree_value_compare_0(i32 %8, i32 %9)
  %11 = icmp eq i8 %10, 0
  br i1 %11, label %comparison_2, label %end_0
comparison_2:
  %12 = getelementptr [3 x i32], [3 x i32]* %lhs_0, i32 0, i32 2
  %13 = getelementptr [3 x i32], [3 x i32]* %rhs_0, i32 0, i32 2
  %14 = load i32, i32* %12
  %15 = load i32, i32* %13
  %16 = call ccc i8 @eclair_btree_value_compare_0(i32 %14, i32 %15)
  br label %end_0
end_0:
  %17 = phi i8 [%4, %comparison_0], [%10, %comparison_1], [%16, %comparison_2]
  ret i8 %17
}

define external ccc %node_t_0* @eclair_btree_node_new_0(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 424, i32 256
  %1 = call ccc i8* @malloc(i32 %0)
  %2 = bitcast i8* %1 to %node_t_0*
  %3 = getelementptr %node_t_0, %node_t_0* %2, i32 0, i32 0, i32 0
  store %node_t_0* zeroinitializer, %node_t_0** %3
  %4 = getelementptr %node_t_0, %node_t_0* %2, i32 0, i32 0, i32 1
  store i16 0, i16* %4
  %5 = getelementptr %node_t_0, %node_t_0* %2, i32 0, i32 0, i32 2
  store i16 0, i16* %5
  %6 = getelementptr %node_t_0, %node_t_0* %2, i32 0, i32 0, i32 3
  store i1 %type_0, i1* %6
  %7 = getelementptr %node_t_0, %node_t_0* %2, i32 0, i32 1
  %8 = bitcast [20 x [3 x i32]]* %7 to i8*
  call ccc void @llvm.memset.p0i8.i64(i8* %8, i8 0, i64 240, i1 0)
  %9 = icmp eq i1 %type_0, 1
  br i1 %9, label %if_0, label %end_if_0
if_0:
  %10 = bitcast %node_t_0* %2 to %inner_node_t_0*
  %11 = getelementptr %inner_node_t_0, %inner_node_t_0* %10, i32 0, i32 1
  %12 = bitcast [21 x %node_t_0*]* %11 to i8*
  call ccc void @llvm.memset.p0i8.i64(i8* %12, i8 0, i64 168, i1 0)
  br label %end_if_0
end_if_0:
  ret %node_t_0* %2
}

define external ccc i64 @eclair_btree_node_count_entries_0(%node_t_0* %node_0) {
start:
  %0 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 2
  %1 = load i16, i16* %0
  %2 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 3
  %3 = load i1, i1* %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  %6 = bitcast %node_t_0* %node_0 to %inner_node_t_0*
  %7 = alloca i64
  store i64 %5, i64* %7
  %8 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 2
  %9 = load i16, i16* %8
  br label %for_begin_0
for_begin_0:
  %10 = phi i16 [0, %end_if_0], [%17, %for_body_0]
  %11 = icmp ule i16 %10, %9
  br i1 %11, label %for_body_0, label %for_end_0
for_body_0:
  %12 = load i64, i64* %7
  %13 = getelementptr %inner_node_t_0, %inner_node_t_0* %6, i32 0, i32 1, i16 %10
  %14 = load %node_t_0*, %node_t_0** %13
  %15 = call ccc i64 @eclair_btree_node_count_entries_0(%node_t_0* %14)
  %16 = add i64 %12, %15
  store i64 %16, i64* %7
  %17 = add i16 1, %10
  br label %for_begin_0
for_end_0:
  %18 = load i64, i64* %7
  ret i64 %18
}

define external ccc void @eclair_btree_iterator_init_0(%btree_iterator_t_0* %iter_0, %node_t_0* %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 0
  store %node_t_0* %cur_0, %node_t_0** %0
  %1 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 1
  store i16 %pos_0, i16* %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_0(%btree_iterator_t_0* %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_0(%btree_iterator_t_0* %iter_0, %node_t_0* zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_0(%btree_iterator_t_0* %lhs_0, %btree_iterator_t_0* %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %lhs_0, i32 0, i32 0
  %1 = load %node_t_0*, %node_t_0** %0
  %2 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %rhs_0, i32 0, i32 0
  %3 = load %node_t_0*, %node_t_0** %2
  %4 = icmp ne %node_t_0* %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %lhs_0, i32 0, i32 1
  %6 = load i16, i16* %5
  %7 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %rhs_0, i32 0, i32 1
  %8 = load i16, i16* %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc [3 x i32]* @eclair_btree_iterator_current_0(%btree_iterator_t_0* %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 1
  %1 = load i16, i16* %0
  %2 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 0
  %3 = load %node_t_0*, %node_t_0** %2
  %4 = getelementptr %node_t_0, %node_t_0* %3, i32 0, i32 1, i16 %1
  ret [3 x i32]* %4
}

define external ccc void @eclair_btree_iterator_next_0(%btree_iterator_t_0* %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 0
  %1 = load %node_t_0*, %node_t_0** %0
  %2 = getelementptr %node_t_0, %node_t_0* %1, i32 0, i32 0, i32 3
  %3 = load i1, i1* %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 1
  %6 = load i16, i16* %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 0
  %9 = load %node_t_0*, %node_t_0** %8
  %10 = bitcast %node_t_0* %9 to %inner_node_t_0*
  %11 = getelementptr %inner_node_t_0, %inner_node_t_0* %10, i32 0, i32 1, i16 %7
  %12 = load %node_t_0*, %node_t_0** %11
  %13 = alloca %node_t_0*
  store %node_t_0* %12, %node_t_0** %13
  br label %while_begin_0
while_begin_0:
  %14 = load %node_t_0*, %node_t_0** %13
  %15 = getelementptr %node_t_0, %node_t_0* %14, i32 0, i32 0, i32 3
  %16 = load i1, i1* %15
  %17 = icmp eq i1 %16, 1
  br i1 %17, label %while_body_0, label %while_end_0
while_body_0:
  %18 = load %node_t_0*, %node_t_0** %13
  %19 = bitcast %node_t_0* %18 to %inner_node_t_0*
  %20 = getelementptr %inner_node_t_0, %inner_node_t_0* %19, i32 0, i32 1, i16 0
  %21 = load %node_t_0*, %node_t_0** %20
  store %node_t_0* %21, %node_t_0** %13
  br label %while_begin_0
while_end_0:
  %22 = load %node_t_0*, %node_t_0** %13
  %23 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 0
  store %node_t_0* %22, %node_t_0** %23
  %24 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 1
  store i16 0, i16* %24
  %25 = getelementptr %node_t_0, %node_t_0* %22, i32 0, i32 0, i32 2
  %26 = load i16, i16* %25
  %27 = icmp ne i16 %26, 0
  br i1 %27, label %if_1, label %end_if_0
if_1:
  ret void
end_if_0:
  br label %leaf.next_0
end_if_1:
  br label %leaf.next_0
leaf.next_0:
  %28 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 1
  %29 = load i16, i16* %28
  %30 = add i16 1, %29
  store i16 %30, i16* %28
  %31 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 1
  %32 = load i16, i16* %31
  %33 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 0
  %34 = load %node_t_0*, %node_t_0** %33
  %35 = getelementptr %node_t_0, %node_t_0* %34, i32 0, i32 0, i32 2
  %36 = load i16, i16* %35
  %37 = icmp ult i16 %32, %36
  br i1 %37, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %38 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 0
  %39 = load %node_t_0*, %node_t_0** %38
  %40 = icmp eq %node_t_0* %39, zeroinitializer
  br i1 %40, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %41 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 1
  %42 = load i16, i16* %41
  %43 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 0
  %44 = load %node_t_0*, %node_t_0** %43
  %45 = getelementptr %node_t_0, %node_t_0* %44, i32 0, i32 0, i32 2
  %46 = load i16, i16* %45
  %47 = icmp eq i16 %42, %46
  br label %loop.condition.end_0
loop.condition.end_0:
  %48 = phi i1 [0, %leaf.no_parent_0], [%47, %leaf.has_parent_0]
  br i1 %48, label %while_body_1, label %while_end_1
while_body_1:
  %49 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 0
  %50 = load %node_t_0*, %node_t_0** %49
  %51 = getelementptr %node_t_0, %node_t_0* %50, i32 0, i32 0, i32 1
  %52 = load i16, i16* %51
  %53 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 1
  store i16 %52, i16* %53
  %54 = getelementptr %node_t_0, %node_t_0* %50, i32 0, i32 0, i32 0
  %55 = load %node_t_0*, %node_t_0** %54
  %56 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %iter_0, i32 0, i32 0
  store %node_t_0* %55, %node_t_0** %56
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc [3 x i32]* @eclair_btree_linear_search_lower_bound_0([3 x i32]* %val_0, [3 x i32]* %current_0, [3 x i32]* %end_0) {
start:
  %0 = alloca [3 x i32]*
  store [3 x i32]* %current_0, [3 x i32]** %0
  br label %while_begin_0
while_begin_0:
  %1 = load [3 x i32]*, [3 x i32]** %0
  %2 = icmp ne [3 x i32]* %1, %end_0
  br i1 %2, label %while_body_0, label %while_end_0
while_body_0:
  %3 = load [3 x i32]*, [3 x i32]** %0
  %4 = call ccc i8 @eclair_btree_value_compare_values_0([3 x i32]* %3, [3 x i32]* %val_0)
  %5 = icmp ne i8 %4, -1
  br i1 %5, label %if_0, label %end_if_0
if_0:
  ret [3 x i32]* %3
end_if_0:
  %6 = getelementptr [3 x i32], [3 x i32]* %3, i32 1
  store [3 x i32]* %6, [3 x i32]** %0
  br label %while_begin_0
while_end_0:
  ret [3 x i32]* %end_0
}

define external ccc [3 x i32]* @eclair_btree_linear_search_upper_bound_0([3 x i32]* %val_0, [3 x i32]* %current_0, [3 x i32]* %end_0) {
start:
  %0 = alloca [3 x i32]*
  store [3 x i32]* %current_0, [3 x i32]** %0
  br label %while_begin_0
while_begin_0:
  %1 = load [3 x i32]*, [3 x i32]** %0
  %2 = icmp ne [3 x i32]* %1, %end_0
  br i1 %2, label %while_body_0, label %while_end_0
while_body_0:
  %3 = load [3 x i32]*, [3 x i32]** %0
  %4 = call ccc i8 @eclair_btree_value_compare_values_0([3 x i32]* %3, [3 x i32]* %val_0)
  %5 = icmp eq i8 %4, 1
  br i1 %5, label %if_0, label %end_if_0
if_0:
  ret [3 x i32]* %3
end_if_0:
  %6 = getelementptr [3 x i32], [3 x i32]* %3, i32 1
  store [3 x i32]* %6, [3 x i32]** %0
  br label %while_begin_0
while_end_0:
  ret [3 x i32]* %end_0
}

define external ccc void @eclair_btree_init_empty_0(%btree_t_0* %tree_0) {
start:
  %0 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 0
  store %node_t_0* zeroinitializer, %node_t_0** %0
  %1 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 1
  store %node_t_0* zeroinitializer, %node_t_0** %1
  ret void
}

define external ccc void @eclair_btree_init_0(%btree_t_0* %tree_0, %btree_iterator_t_0* %start_0, %btree_iterator_t_0* %end_0) {
start:
  call ccc void @eclair_btree_insert_range__0(%btree_t_0* %tree_0, %btree_iterator_t_0* %start_0, %btree_iterator_t_0* %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_0(%btree_t_0* %tree_0) {
start:
  call ccc void @eclair_btree_clear_0(%btree_t_0* %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_0(%btree_t_0* %tree_0) {
start:
  %0 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 0
  %1 = load %node_t_0*, %node_t_0** %0
  %2 = icmp eq %node_t_0* %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_0(%btree_t_0* %tree_0) {
start:
  %0 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 0
  %1 = load %node_t_0*, %node_t_0** %0
  %2 = icmp eq %node_t_0* %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_0(%node_t_0* %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_0() {
start:
  %0 = mul i16 3, 20
  %1 = udiv i16 %0, 4
  %2 = sub i16 20, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_0(%node_t_0* %node_0, %node_t_0** %root_0) {
start:
  %0 = call ccc i16 @eclair_btree_node_split_point_0()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 3
  %3 = load i1, i1* %2
  %4 = call ccc %node_t_0* @eclair_btree_node_new_0(i1 %3)
  %5 = alloca i16
  store i16 0, i16* %5
  br label %for_begin_0
for_begin_0:
  %6 = phi i16 [%1, %start], [%13, %for_body_0]
  %7 = icmp ult i16 %6, 20
  br i1 %7, label %for_body_0, label %for_end_0
for_body_0:
  %8 = load i16, i16* %5
  %9 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 1, i16 %6
  %10 = load [3 x i32], [3 x i32]* %9
  %11 = getelementptr %node_t_0, %node_t_0* %4, i32 0, i32 1, i16 %8
  store [3 x i32] %10, [3 x i32]* %11
  %12 = add i16 1, %8
  store i16 %12, i16* %5
  %13 = add i16 1, %6
  br label %for_begin_0
for_end_0:
  %14 = icmp eq i1 %3, 1
  br i1 %14, label %if_0, label %end_if_0
if_0:
  %15 = bitcast %node_t_0* %4 to %inner_node_t_0*
  %16 = bitcast %node_t_0* %node_0 to %inner_node_t_0*
  store i16 0, i16* %5
  br label %for_begin_1
for_begin_1:
  %17 = phi i16 [%1, %if_0], [%26, %for_body_1]
  %18 = icmp ule i16 %17, 20
  br i1 %18, label %for_body_1, label %for_end_1
for_body_1:
  %19 = load i16, i16* %5
  %20 = getelementptr %inner_node_t_0, %inner_node_t_0* %16, i32 0, i32 1, i16 %17
  %21 = load %node_t_0*, %node_t_0** %20
  %22 = getelementptr %node_t_0, %node_t_0* %21, i32 0, i32 0, i32 0
  store %node_t_0* %4, %node_t_0** %22
  %23 = getelementptr %node_t_0, %node_t_0* %21, i32 0, i32 0, i32 1
  store i16 %19, i16* %23
  %24 = getelementptr %inner_node_t_0, %inner_node_t_0* %15, i32 0, i32 1, i16 %19
  store %node_t_0* %21, %node_t_0** %24
  %25 = add i16 1, %19
  store i16 %25, i16* %5
  %26 = add i16 1, %17
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %27 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 2
  store i16 %0, i16* %27
  %28 = sub i16 20, %0
  %29 = sub i16 %28, 1
  %30 = getelementptr %node_t_0, %node_t_0* %4, i32 0, i32 0, i32 2
  store i16 %29, i16* %30
  call ccc void @eclair_btree_node_grow_parent_0(%node_t_0* %node_0, %node_t_0** %root_0, %node_t_0* %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_0(%node_t_0* %node_0, %node_t_0** %root_0, %node_t_0* %sibling_0) {
start:
  %0 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 0
  %1 = load %node_t_0*, %node_t_0** %0
  %2 = icmp eq %node_t_0* %1, zeroinitializer
  %3 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 2
  %4 = load i16, i16* %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc %node_t_0* @eclair_btree_node_new_0(i1 1)
  %6 = bitcast %node_t_0* %5 to %inner_node_t_0*
  %7 = getelementptr %node_t_0, %node_t_0* %5, i32 0, i32 0, i32 2
  store i16 1, i16* %7
  %8 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 1, i16 %4
  %9 = load [3 x i32], [3 x i32]* %8
  %10 = getelementptr %node_t_0, %node_t_0* %5, i32 0, i32 1, i16 0
  store [3 x i32] %9, [3 x i32]* %10
  %11 = getelementptr %inner_node_t_0, %inner_node_t_0* %6, i32 0, i32 1, i16 0
  store %node_t_0* %node_0, %node_t_0** %11
  %12 = getelementptr %inner_node_t_0, %inner_node_t_0* %6, i32 0, i32 1, i16 1
  store %node_t_0* %sibling_0, %node_t_0** %12
  %13 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 0
  store %node_t_0* %5, %node_t_0** %13
  %14 = getelementptr %node_t_0, %node_t_0* %sibling_0, i32 0, i32 0, i32 0
  store %node_t_0* %5, %node_t_0** %14
  %15 = getelementptr %node_t_0, %node_t_0* %sibling_0, i32 0, i32 0, i32 1
  store i16 1, i16* %15
  store %node_t_0* %5, %node_t_0** %root_0
  ret void
insert_new_node_in_parent_0:
  %16 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 1
  %17 = load i16, i16* %16
  %18 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_0(%node_t_0* %1, %node_t_0** %root_0, i16 %17, %node_t_0* %node_0, [3 x i32]* %18, %node_t_0* %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_0(%node_t_0* %node_0, %node_t_0** %root_0, i16 %pos_0, %node_t_0* %predecessor_0, [3 x i32]* %key_0, %node_t_0* %new_node_0) {
start:
  %0 = alloca i16
  store i16 %pos_0, i16* %0
  %1 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 2
  %2 = load i16, i16* %1
  %3 = icmp uge i16 %2, 20
  br i1 %3, label %if_0, label %end_if_1
if_0:
  %4 = load i16, i16* %0
  %5 = call ccc i16 @eclair_btree_node_rebalance_or_split_0(%node_t_0* %node_0, %node_t_0** %root_0, i16 %pos_0)
  %6 = sub i16 %4, %5
  store i16 %6, i16* %0
  %7 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 2
  %8 = load i16, i16* %7
  %9 = icmp ugt i16 %6, %8
  br i1 %9, label %if_1, label %end_if_0
if_1:
  %10 = sub i16 %6, %8
  %11 = sub i16 %10, 1
  store i16 %11, i16* %0
  %12 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 0
  %13 = load %node_t_0*, %node_t_0** %12
  %14 = bitcast %node_t_0* %13 to %inner_node_t_0*
  %15 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 1
  %16 = load i16, i16* %15
  %17 = add i16 1, %16
  %18 = getelementptr %inner_node_t_0, %inner_node_t_0* %14, i32 0, i32 1, i16 %17
  %19 = load %node_t_0*, %node_t_0** %18
  call ccc void @eclair_btree_node_insert_inner_0(%node_t_0* %19, %node_t_0** %root_0, i16 %11, %node_t_0* %predecessor_0, [3 x i32]* %key_0, %node_t_0* %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %20 = bitcast %node_t_0* %node_0 to %inner_node_t_0*
  %21 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 2
  %22 = load i16, i16* %21
  %23 = sub i16 %22, 1
  %24 = load i16, i16* %0
  br label %for_begin_0
for_begin_0:
  %25 = phi i16 [%23, %end_if_1], [%40, %for_body_0]
  %26 = icmp uge i16 %25, %24
  br i1 %26, label %for_body_0, label %for_end_0
for_body_0:
  %27 = add i16 %25, 1
  %28 = add i16 %25, 2
  %29 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 1, i16 %25
  %30 = load [3 x i32], [3 x i32]* %29
  %31 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 1, i16 %27
  store [3 x i32] %30, [3 x i32]* %31
  %32 = getelementptr %inner_node_t_0, %inner_node_t_0* %20, i32 0, i32 1, i16 %27
  %33 = load %node_t_0*, %node_t_0** %32
  %34 = getelementptr %inner_node_t_0, %inner_node_t_0* %20, i32 0, i32 1, i16 %28
  store %node_t_0* %33, %node_t_0** %34
  %35 = getelementptr %inner_node_t_0, %inner_node_t_0* %20, i32 0, i32 1, i16 %28
  %36 = load %node_t_0*, %node_t_0** %35
  %37 = getelementptr %node_t_0, %node_t_0* %36, i32 0, i32 0, i32 1
  %38 = load i16, i16* %37
  %39 = add i16 1, %38
  store i16 %39, i16* %37
  %40 = sub i16 %25, 1
  br label %for_begin_0
for_end_0:
  %41 = load [3 x i32], [3 x i32]* %key_0
  %42 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 1, i16 %24
  store [3 x i32] %41, [3 x i32]* %42
  %43 = add i16 %24, 1
  %44 = getelementptr %inner_node_t_0, %inner_node_t_0* %20, i32 0, i32 1, i16 %43
  store %node_t_0* %new_node_0, %node_t_0** %44
  %45 = getelementptr %node_t_0, %node_t_0* %new_node_0, i32 0, i32 0, i32 0
  store %node_t_0* %node_0, %node_t_0** %45
  %46 = getelementptr %node_t_0, %node_t_0* %new_node_0, i32 0, i32 0, i32 1
  store i16 %43, i16* %46
  %47 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 2
  %48 = load i16, i16* %47
  %49 = add i16 1, %48
  store i16 %49, i16* %47
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_0(%node_t_0* %node_0, %node_t_0** %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 0
  %1 = load %node_t_0*, %node_t_0** %0
  %2 = bitcast %node_t_0* %1 to %inner_node_t_0*
  %3 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 1
  %4 = load i16, i16* %3
  %5 = icmp ne %inner_node_t_0* %2, zeroinitializer
  %6 = icmp ugt i16 %4, 0
  %7 = and i1 %5, %6
  br i1 %7, label %rebalance_0, label %split_0
rebalance_0:
  %8 = sub i16 %4, 1
  %9 = getelementptr %inner_node_t_0, %inner_node_t_0* %2, i32 0, i32 1, i16 %8
  %10 = load %node_t_0*, %node_t_0** %9
  %11 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 0, i32 2
  %12 = load i16, i16* %11
  %13 = sub i16 20, %12
  %14 = icmp slt i16 %13, %idx_0
  %15 = select i1 %14, i16 %13, i16 %idx_0
  %16 = icmp ugt i16 %15, 0
  br i1 %16, label %if_0, label %end_if_1
if_0:
  %17 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 1
  %18 = load i16, i16* %17
  %19 = sub i16 %18, 1
  %20 = getelementptr %inner_node_t_0, %inner_node_t_0* %2, i32 0, i32 0, i32 1, i16 %19
  %21 = load [3 x i32], [3 x i32]* %20
  %22 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 0, i32 2
  %23 = load i16, i16* %22
  %24 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 1, i16 %23
  store [3 x i32] %21, [3 x i32]* %24
  %25 = sub i16 %15, 1
  br label %for_begin_0
for_begin_0:
  %26 = phi i16 [0, %if_0], [%33, %for_body_0]
  %27 = icmp ult i16 %26, %25
  br i1 %27, label %for_body_0, label %for_end_0
for_body_0:
  %28 = add i16 %23, 1
  %29 = add i16 %26, %28
  %30 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 1, i16 %26
  %31 = load [3 x i32], [3 x i32]* %30
  %32 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 1, i16 %29
  store [3 x i32] %31, [3 x i32]* %32
  %33 = add i16 1, %26
  br label %for_begin_0
for_end_0:
  %34 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 1, i16 %25
  %35 = load [3 x i32], [3 x i32]* %34
  store [3 x i32] %35, [3 x i32]* %20
  %36 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 2
  %37 = load i16, i16* %36
  %38 = sub i16 %37, %15
  br label %for_begin_1
for_begin_1:
  %39 = phi i16 [0, %for_end_0], [%45, %for_body_1]
  %40 = icmp ult i16 %39, %38
  br i1 %40, label %for_body_1, label %for_end_1
for_body_1:
  %41 = add i16 %39, %15
  %42 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 1, i16 %41
  %43 = load [3 x i32], [3 x i32]* %42
  %44 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 1, i16 %39
  store [3 x i32] %43, [3 x i32]* %44
  %45 = add i16 1, %39
  br label %for_begin_1
for_end_1:
  %46 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 3
  %47 = load i1, i1* %46
  %48 = icmp eq i1 %47, 1
  br i1 %48, label %if_1, label %end_if_0
if_1:
  %49 = bitcast %node_t_0* %node_0 to %inner_node_t_0*
  %50 = bitcast %node_t_0* %10 to %inner_node_t_0*
  br label %for_begin_2
for_begin_2:
  %51 = phi i16 [0, %if_1], [%64, %for_body_2]
  %52 = icmp ult i16 %51, %15
  br i1 %52, label %for_body_2, label %for_end_2
for_body_2:
  %53 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 0, i32 2
  %54 = load i16, i16* %53
  %55 = add i16 %54, 1
  %56 = add i16 %51, %55
  %57 = getelementptr %inner_node_t_0, %inner_node_t_0* %49, i32 0, i32 1, i16 %51
  %58 = load %node_t_0*, %node_t_0** %57
  %59 = getelementptr %inner_node_t_0, %inner_node_t_0* %50, i32 0, i32 1, i16 %56
  store %node_t_0* %58, %node_t_0** %59
  %60 = getelementptr %inner_node_t_0, %inner_node_t_0* %50, i32 0, i32 1, i16 %56
  %61 = load %node_t_0*, %node_t_0** %60
  %62 = getelementptr %node_t_0, %node_t_0* %61, i32 0, i32 0, i32 0
  store %node_t_0* %10, %node_t_0** %62
  %63 = getelementptr %node_t_0, %node_t_0* %61, i32 0, i32 0, i32 1
  store i16 %56, i16* %63
  %64 = add i16 1, %51
  br label %for_begin_2
for_end_2:
  %65 = sub i16 %37, %15
  %66 = add i16 1, %65
  br label %for_begin_3
for_begin_3:
  %67 = phi i16 [0, %for_end_2], [%76, %for_body_3]
  %68 = icmp ult i16 %67, %66
  br i1 %68, label %for_body_3, label %for_end_3
for_body_3:
  %69 = add i16 %67, %15
  %70 = getelementptr %inner_node_t_0, %inner_node_t_0* %49, i32 0, i32 1, i16 %69
  %71 = load %node_t_0*, %node_t_0** %70
  %72 = getelementptr %inner_node_t_0, %inner_node_t_0* %49, i32 0, i32 1, i16 %67
  store %node_t_0* %71, %node_t_0** %72
  %73 = getelementptr %inner_node_t_0, %inner_node_t_0* %49, i32 0, i32 1, i16 %67
  %74 = load %node_t_0*, %node_t_0** %73
  %75 = getelementptr %node_t_0, %node_t_0* %74, i32 0, i32 0, i32 1
  store i16 %67, i16* %75
  %76 = add i16 1, %67
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %77 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 0, i32 2
  %78 = load i16, i16* %77
  %79 = add i16 %78, %15
  store i16 %79, i16* %77
  %80 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 2
  %81 = load i16, i16* %80
  %82 = sub i16 %81, %15
  store i16 %82, i16* %80
  ret i16 %15
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_0(%node_t_0* %node_0, %node_t_0** %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %tree_0, [3 x i32]* %val_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_0(%btree_t_0* %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc %node_t_0* @eclair_btree_node_new_0(i1 0)
  %2 = getelementptr %node_t_0, %node_t_0* %1, i32 0, i32 0, i32 2
  store i16 1, i16* %2
  %3 = load [3 x i32], [3 x i32]* %val_0
  %4 = getelementptr %node_t_0, %node_t_0* %1, i32 0, i32 1, i16 0
  store [3 x i32] %3, [3 x i32]* %4
  %5 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 0
  store %node_t_0* %1, %node_t_0** %5
  %6 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 1
  store %node_t_0* %1, %node_t_0** %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 0
  %8 = load %node_t_0*, %node_t_0** %7
  %9 = alloca %node_t_0*
  store %node_t_0* %8, %node_t_0** %9
  br label %loop_0
loop_0:
  %10 = load %node_t_0*, %node_t_0** %9
  %11 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 0, i32 3
  %12 = load i1, i1* %11
  %13 = icmp eq i1 %12, 1
  br i1 %13, label %inner_0, label %leaf_0
inner_0:
  %14 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 0, i32 2
  %15 = load i16, i16* %14
  %16 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 1, i16 0
  %17 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 1, i16 %15
  %18 = call ccc [3 x i32]* @eclair_btree_linear_search_lower_bound_0([3 x i32]* %val_0, [3 x i32]* %16, [3 x i32]* %17)
  %19 = ptrtoint [3 x i32]* %18 to i64
  %20 = ptrtoint [3 x i32]* %16 to i64
  %21 = sub i64 %19, %20
  %22 = trunc i64 %21 to i16
  %23 = udiv i16 %22, 12
  %24 = icmp ne [3 x i32]* %18, %17
  %25 = call ccc i8 @eclair_btree_value_compare_values_0([3 x i32]* %18, [3 x i32]* %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %28 = bitcast %node_t_0* %10 to %inner_node_t_0*
  %29 = getelementptr %inner_node_t_0, %inner_node_t_0* %28, i32 0, i32 1, i16 %23
  %30 = load %node_t_0*, %node_t_0** %29
  store %node_t_0* %30, %node_t_0** %9
  br label %loop_0
leaf_0:
  %31 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 0, i32 2
  %32 = load i16, i16* %31
  %33 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 1, i16 0
  %34 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 1, i16 %32
  %35 = call ccc [3 x i32]* @eclair_btree_linear_search_upper_bound_0([3 x i32]* %val_0, [3 x i32]* %33, [3 x i32]* %34)
  %36 = ptrtoint [3 x i32]* %35 to i64
  %37 = ptrtoint [3 x i32]* %33 to i64
  %38 = sub i64 %36, %37
  %39 = trunc i64 %38 to i16
  %40 = udiv i16 %39, 12
  %41 = alloca i16
  store i16 %40, i16* %41
  %42 = icmp ne [3 x i32]* %35, %33
  %43 = getelementptr [3 x i32], [3 x i32]* %35, i32 -1
  %44 = call ccc i8 @eclair_btree_value_compare_values_0([3 x i32]* %43, [3 x i32]* %val_0)
  %45 = icmp eq i8 0, %44
  %46 = and i1 %42, %45
  br i1 %46, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %47 = icmp uge i16 %32, 20
  br i1 %47, label %split_0, label %no_split_0
split_0:
  %48 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 0
  %49 = load i16, i16* %41
  %50 = call ccc i16 @eclair_btree_node_rebalance_or_split_0(%node_t_0* %10, %node_t_0** %48, i16 %49)
  %51 = sub i16 %49, %50
  store i16 %51, i16* %41
  %52 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 0, i32 2
  %53 = load i16, i16* %52
  %54 = icmp ugt i16 %51, %53
  br i1 %54, label %if_0, label %end_if_0
if_0:
  %55 = add i16 %53, 1
  %56 = sub i16 %51, %55
  store i16 %56, i16* %41
  %57 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 0, i32 0
  %58 = load %node_t_0*, %node_t_0** %57
  %59 = bitcast %node_t_0* %58 to %inner_node_t_0*
  %60 = getelementptr %node_t_0, %node_t_0* %10, i32 0, i32 0, i32 1
  %61 = load i16, i16* %60
  %62 = add i16 1, %61
  %63 = getelementptr %inner_node_t_0, %inner_node_t_0* %59, i32 0, i32 1, i16 %62
  %64 = load %node_t_0*, %node_t_0** %63
  store %node_t_0* %64, %node_t_0** %9
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %65 = load %node_t_0*, %node_t_0** %9
  %66 = load i16, i16* %41
  %67 = getelementptr %node_t_0, %node_t_0* %65, i32 0, i32 0, i32 2
  %68 = load i16, i16* %67
  br label %for_begin_0
for_begin_0:
  %69 = phi i16 [%68, %no_split_0], [%75, %for_body_0]
  %70 = icmp ugt i16 %69, %66
  br i1 %70, label %for_body_0, label %for_end_0
for_body_0:
  %71 = sub i16 %69, 1
  %72 = getelementptr %node_t_0, %node_t_0* %65, i32 0, i32 1, i16 %71
  %73 = load [3 x i32], [3 x i32]* %72
  %74 = getelementptr %node_t_0, %node_t_0* %65, i32 0, i32 1, i16 %69
  store [3 x i32] %73, [3 x i32]* %74
  %75 = sub i16 %69, 1
  br label %for_begin_0
for_end_0:
  %76 = load [3 x i32], [3 x i32]* %val_0
  %77 = getelementptr %node_t_0, %node_t_0* %65, i32 0, i32 1, i16 %66
  store [3 x i32] %76, [3 x i32]* %77
  %78 = getelementptr %node_t_0, %node_t_0* %65, i32 0, i32 0, i32 2
  %79 = load i16, i16* %78
  %80 = add i16 1, %79
  store i16 %80, i16* %78
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__0(%btree_t_0* %tree_0, %btree_iterator_t_0* %begin_0, %btree_iterator_t_0* %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_0, %btree_iterator_t_0* %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc [3 x i32]* @eclair_btree_iterator_current_0(%btree_iterator_t_0* %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %tree_0, [3 x i32]* %2)
  call ccc void @eclair_btree_iterator_next_0(%btree_iterator_t_0* %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_0(%btree_t_0* %tree_0, %btree_iterator_t_0* %result_0) {
start:
  %0 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 1
  %1 = load %node_t_0*, %node_t_0** %0
  %2 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %result_0, i32 0, i32 0
  store %node_t_0* %1, %node_t_0** %2
  %3 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %result_0, i32 0, i32 1
  store i16 0, i16* %3
  ret void
}

define external ccc void @eclair_btree_end_0(%btree_t_0* %tree_0, %btree_iterator_t_0* %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_0(%btree_iterator_t_0* %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_0(%btree_t_0* %tree_0, [3 x i32]* %val_0) {
start:
  %0 = alloca %btree_iterator_t_0, i32 1
  %1 = alloca %btree_iterator_t_0, i32 1
  call ccc void @eclair_btree_find_0(%btree_t_0* %tree_0, [3 x i32]* %val_0, %btree_iterator_t_0* %0)
  call ccc void @eclair_btree_end_0(%btree_t_0* %tree_0, %btree_iterator_t_0* %1)
  %2 = call ccc i1 @eclair_btree_iterator_is_equal_0(%btree_iterator_t_0* %0, %btree_iterator_t_0* %1)
  %3 = select i1 %2, i1 0, i1 1
  ret i1 %3
}

define external ccc void @eclair_btree_find_0(%btree_t_0* %tree_0, [3 x i32]* %val_0, %btree_iterator_t_0* %result_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_0(%btree_t_0* %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_0(%btree_iterator_t_0* %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 0
  %2 = load %node_t_0*, %node_t_0** %1
  %3 = alloca %node_t_0*
  store %node_t_0* %2, %node_t_0** %3
  br label %loop_0
loop_0:
  %4 = load %node_t_0*, %node_t_0** %3
  %5 = getelementptr %node_t_0, %node_t_0* %4, i32 0, i32 0, i32 2
  %6 = load i16, i16* %5
  %7 = getelementptr %node_t_0, %node_t_0* %4, i32 0, i32 1, i16 0
  %8 = getelementptr %node_t_0, %node_t_0* %4, i32 0, i32 1, i16 %6
  %9 = call ccc [3 x i32]* @eclair_btree_linear_search_lower_bound_0([3 x i32]* %val_0, [3 x i32]* %7, [3 x i32]* %8)
  %10 = ptrtoint [3 x i32]* %9 to i64
  %11 = ptrtoint [3 x i32]* %7 to i64
  %12 = sub i64 %10, %11
  %13 = trunc i64 %12 to i16
  %14 = udiv i16 %13, 12
  %15 = icmp ult [3 x i32]* %9, %8
  %16 = call ccc i8 @eclair_btree_value_compare_values_0([3 x i32]* %9, [3 x i32]* %val_0)
  %17 = icmp eq i8 0, %16
  %18 = and i1 %15, %17
  br i1 %18, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_0(%btree_iterator_t_0* %result_0, %node_t_0* %4, i16 %14)
  ret void
end_if_1:
  %19 = getelementptr %node_t_0, %node_t_0* %4, i32 0, i32 0, i32 3
  %20 = load i1, i1* %19
  %21 = icmp eq i1 %20, 0
  br i1 %21, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_0(%btree_iterator_t_0* %result_0)
  ret void
end_if_2:
  %22 = bitcast %node_t_0* %4 to %inner_node_t_0*
  %23 = getelementptr %inner_node_t_0, %inner_node_t_0* %22, i32 0, i32 1, i16 %14
  %24 = load %node_t_0*, %node_t_0** %23
  store %node_t_0* %24, %node_t_0** %3
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_0(%btree_t_0* %tree_0, [3 x i32]* %val_0, %btree_iterator_t_0* %result_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_0(%btree_t_0* %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_0(%btree_iterator_t_0* %result_0)
  ret void
end_if_0:
  %1 = alloca %btree_iterator_t_0, i32 1
  call ccc void @eclair_btree_iterator_end_init_0(%btree_iterator_t_0* %1)
  %2 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 0
  %3 = load %node_t_0*, %node_t_0** %2
  %4 = alloca %node_t_0*
  store %node_t_0* %3, %node_t_0** %4
  br label %loop_0
loop_0:
  %5 = load %node_t_0*, %node_t_0** %4
  %6 = getelementptr %node_t_0, %node_t_0* %5, i32 0, i32 0, i32 2
  %7 = load i16, i16* %6
  %8 = getelementptr %node_t_0, %node_t_0* %5, i32 0, i32 1, i16 0
  %9 = getelementptr %node_t_0, %node_t_0* %5, i32 0, i32 1, i16 %7
  %10 = call ccc [3 x i32]* @eclair_btree_linear_search_lower_bound_0([3 x i32]* %val_0, [3 x i32]* %8, [3 x i32]* %9)
  %11 = ptrtoint [3 x i32]* %10 to i64
  %12 = ptrtoint [3 x i32]* %8 to i64
  %13 = sub i64 %11, %12
  %14 = trunc i64 %13 to i16
  %15 = udiv i16 %14, 12
  %16 = getelementptr %node_t_0, %node_t_0* %5, i32 0, i32 0, i32 3
  %17 = load i1, i1* %16
  %18 = icmp eq i1 %17, 0
  br i1 %18, label %if_1, label %end_if_1
if_1:
  %19 = icmp eq [3 x i32]* %10, %9
  br i1 %19, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %20 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %1, i32 0, i32 0
  %21 = load %node_t_0*, %node_t_0** %20
  %22 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %result_0, i32 0, i32 0
  store %node_t_0* %21, %node_t_0** %22
  %23 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %1, i32 0, i32 1
  %24 = load i16, i16* %23
  %25 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %result_0, i32 0, i32 1
  store i16 %24, i16* %25
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_0(%btree_iterator_t_0* %result_0, %node_t_0* %5, i16 %15)
  ret void
end_if_1:
  %26 = icmp ne [3 x i32]* %10, %9
  %27 = call ccc i8 @eclair_btree_value_compare_values_0([3 x i32]* %10, [3 x i32]* %val_0)
  %28 = icmp eq i8 0, %27
  %29 = and i1 %26, %28
  br i1 %29, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_0(%btree_iterator_t_0* %result_0, %node_t_0* %5, i16 %15)
  ret void
end_if_2:
  br i1 %26, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_0(%btree_iterator_t_0* %1, %node_t_0* %5, i16 %15)
  br label %end_if_3
end_if_3:
  %30 = bitcast %node_t_0* %5 to %inner_node_t_0*
  %31 = getelementptr %inner_node_t_0, %inner_node_t_0* %30, i32 0, i32 1, i16 %15
  %32 = load %node_t_0*, %node_t_0** %31
  store %node_t_0* %32, %node_t_0** %4
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_0(%btree_t_0* %tree_0, [3 x i32]* %val_0, %btree_iterator_t_0* %result_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_0(%btree_t_0* %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_0(%btree_iterator_t_0* %result_0)
  ret void
end_if_0:
  %1 = alloca %btree_iterator_t_0, i32 1
  call ccc void @eclair_btree_iterator_end_init_0(%btree_iterator_t_0* %1)
  %2 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 0
  %3 = load %node_t_0*, %node_t_0** %2
  %4 = alloca %node_t_0*
  store %node_t_0* %3, %node_t_0** %4
  br label %loop_0
loop_0:
  %5 = load %node_t_0*, %node_t_0** %4
  %6 = getelementptr %node_t_0, %node_t_0* %5, i32 0, i32 0, i32 2
  %7 = load i16, i16* %6
  %8 = getelementptr %node_t_0, %node_t_0* %5, i32 0, i32 1, i16 0
  %9 = getelementptr %node_t_0, %node_t_0* %5, i32 0, i32 1, i16 %7
  %10 = call ccc [3 x i32]* @eclair_btree_linear_search_upper_bound_0([3 x i32]* %val_0, [3 x i32]* %8, [3 x i32]* %9)
  %11 = ptrtoint [3 x i32]* %10 to i64
  %12 = ptrtoint [3 x i32]* %8 to i64
  %13 = sub i64 %11, %12
  %14 = trunc i64 %13 to i16
  %15 = udiv i16 %14, 12
  %16 = getelementptr %node_t_0, %node_t_0* %5, i32 0, i32 0, i32 3
  %17 = load i1, i1* %16
  %18 = icmp eq i1 %17, 0
  br i1 %18, label %if_1, label %end_if_1
if_1:
  %19 = icmp eq [3 x i32]* %10, %9
  br i1 %19, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %20 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %1, i32 0, i32 0
  %21 = load %node_t_0*, %node_t_0** %20
  %22 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %result_0, i32 0, i32 0
  store %node_t_0* %21, %node_t_0** %22
  %23 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %1, i32 0, i32 1
  %24 = load i16, i16* %23
  %25 = getelementptr %btree_iterator_t_0, %btree_iterator_t_0* %result_0, i32 0, i32 1
  store i16 %24, i16* %25
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_0(%btree_iterator_t_0* %result_0, %node_t_0* %5, i16 %15)
  ret void
end_if_1:
  %26 = icmp ne [3 x i32]* %10, %9
  br i1 %26, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_0(%btree_iterator_t_0* %result_0, %node_t_0* %5, i16 %15)
  br label %end_if_2
end_if_2:
  %27 = bitcast %node_t_0* %5 to %inner_node_t_0*
  %28 = getelementptr %inner_node_t_0, %inner_node_t_0* %27, i32 0, i32 1, i16 %15
  %29 = load %node_t_0*, %node_t_0** %28
  store %node_t_0* %29, %node_t_0** %4
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_0(%node_t_0* %node_0) {
start:
  %0 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 3
  %1 = load i1, i1* %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = bitcast %node_t_0* %node_0 to %inner_node_t_0*
  %4 = getelementptr %node_t_0, %node_t_0* %node_0, i32 0, i32 0, i32 2
  %5 = load i16, i16* %4
  br label %for_begin_0
for_begin_0:
  %6 = phi i16 [0, %if_0], [%11, %end_if_0]
  %7 = icmp ule i16 %6, %5
  br i1 %7, label %for_body_0, label %for_end_0
for_body_0:
  %8 = getelementptr %inner_node_t_0, %inner_node_t_0* %3, i32 0, i32 1, i16 %6
  %9 = load %node_t_0*, %node_t_0** %8
  %10 = icmp ne %node_t_0* %9, zeroinitializer
  br i1 %10, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_0(%node_t_0* %9)
  br label %end_if_0
end_if_0:
  %11 = add i16 1, %6
  br label %for_begin_0
for_end_0:
  br label %end_if_1
end_if_1:
  %12 = bitcast %node_t_0* %node_0 to i8*
  call ccc void @free(i8* %12)
  ret void
}

define external ccc void @eclair_btree_clear_0(%btree_t_0* %tree_0) {
start:
  %0 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 0
  %1 = load %node_t_0*, %node_t_0** %0
  %2 = icmp ne %node_t_0* %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_0(%node_t_0* %1)
  %3 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 0
  store %node_t_0* zeroinitializer, %node_t_0** %3
  %4 = getelementptr %btree_t_0, %btree_t_0* %tree_0, i32 0, i32 1
  store %node_t_0* zeroinitializer, %node_t_0** %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_0(%btree_t_0* %lhs_0, %btree_t_0* %rhs_0) {
start:
  %0 = getelementptr %btree_t_0, %btree_t_0* %lhs_0, i32 0, i32 0
  %1 = load %node_t_0*, %node_t_0** %0
  %2 = getelementptr %btree_t_0, %btree_t_0* %rhs_0, i32 0, i32 0
  %3 = load %node_t_0*, %node_t_0** %2
  %4 = getelementptr %btree_t_0, %btree_t_0* %lhs_0, i32 0, i32 0
  store %node_t_0* %3, %node_t_0** %4
  %5 = getelementptr %btree_t_0, %btree_t_0* %rhs_0, i32 0, i32 0
  store %node_t_0* %1, %node_t_0** %5
  %6 = getelementptr %btree_t_0, %btree_t_0* %lhs_0, i32 0, i32 1
  %7 = load %node_t_0*, %node_t_0** %6
  %8 = getelementptr %btree_t_0, %btree_t_0* %rhs_0, i32 0, i32 1
  %9 = load %node_t_0*, %node_t_0** %8
  %10 = getelementptr %btree_t_0, %btree_t_0* %lhs_0, i32 0, i32 1
  store %node_t_0* %9, %node_t_0** %10
  %11 = getelementptr %btree_t_0, %btree_t_0* %rhs_0, i32 0, i32 1
  store %node_t_0* %7, %node_t_0** %11
  ret void
}

%node_data_t_1 = type {%node_t_1*, i16, i16, i1}

%node_t_1 = type {%node_data_t_1, [30 x [2 x i32]]}

%inner_node_t_1 = type {%node_t_1, [31 x %node_t_1*]}

%btree_iterator_t_1 = type {%node_t_1*, i16}

%btree_t_1 = type {%node_t_1*, %node_t_1*}

define external ccc i8 @eclair_btree_value_compare_1(i32 %lhs_0, i32 %rhs_0) {
start:
  %0 = icmp ult i32 %lhs_0, %rhs_0
  br i1 %0, label %if_0, label %end_if_0
if_0:
  ret i8 -1
end_if_0:
  %1 = icmp ugt i32 %lhs_0, %rhs_0
  %2 = select i1 %1, i8 1, i8 0
  ret i8 %2
}

define external ccc i8 @eclair_btree_value_compare_values_1([2 x i32]* %lhs_0, [2 x i32]* %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [2 x i32], [2 x i32]* %lhs_0, i32 0, i32 0
  %1 = getelementptr [2 x i32], [2 x i32]* %rhs_0, i32 0, i32 0
  %2 = load i32, i32* %0
  %3 = load i32, i32* %1
  %4 = call ccc i8 @eclair_btree_value_compare_1(i32 %2, i32 %3)
  %5 = icmp eq i8 %4, 0
  br i1 %5, label %comparison_1, label %end_0
comparison_1:
  %6 = getelementptr [2 x i32], [2 x i32]* %lhs_0, i32 0, i32 1
  %7 = getelementptr [2 x i32], [2 x i32]* %rhs_0, i32 0, i32 1
  %8 = load i32, i32* %6
  %9 = load i32, i32* %7
  %10 = call ccc i8 @eclair_btree_value_compare_1(i32 %8, i32 %9)
  br label %end_0
end_0:
  %11 = phi i8 [%4, %comparison_0], [%10, %comparison_1]
  ret i8 %11
}

define external ccc %node_t_1* @eclair_btree_node_new_1(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 504, i32 256
  %1 = call ccc i8* @malloc(i32 %0)
  %2 = bitcast i8* %1 to %node_t_1*
  %3 = getelementptr %node_t_1, %node_t_1* %2, i32 0, i32 0, i32 0
  store %node_t_1* zeroinitializer, %node_t_1** %3
  %4 = getelementptr %node_t_1, %node_t_1* %2, i32 0, i32 0, i32 1
  store i16 0, i16* %4
  %5 = getelementptr %node_t_1, %node_t_1* %2, i32 0, i32 0, i32 2
  store i16 0, i16* %5
  %6 = getelementptr %node_t_1, %node_t_1* %2, i32 0, i32 0, i32 3
  store i1 %type_0, i1* %6
  %7 = getelementptr %node_t_1, %node_t_1* %2, i32 0, i32 1
  %8 = bitcast [30 x [2 x i32]]* %7 to i8*
  call ccc void @llvm.memset.p0i8.i64(i8* %8, i8 0, i64 240, i1 0)
  %9 = icmp eq i1 %type_0, 1
  br i1 %9, label %if_0, label %end_if_0
if_0:
  %10 = bitcast %node_t_1* %2 to %inner_node_t_1*
  %11 = getelementptr %inner_node_t_1, %inner_node_t_1* %10, i32 0, i32 1
  %12 = bitcast [31 x %node_t_1*]* %11 to i8*
  call ccc void @llvm.memset.p0i8.i64(i8* %12, i8 0, i64 248, i1 0)
  br label %end_if_0
end_if_0:
  ret %node_t_1* %2
}

define external ccc i64 @eclair_btree_node_count_entries_1(%node_t_1* %node_0) {
start:
  %0 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 2
  %1 = load i16, i16* %0
  %2 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 3
  %3 = load i1, i1* %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  %6 = bitcast %node_t_1* %node_0 to %inner_node_t_1*
  %7 = alloca i64
  store i64 %5, i64* %7
  %8 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 2
  %9 = load i16, i16* %8
  br label %for_begin_0
for_begin_0:
  %10 = phi i16 [0, %end_if_0], [%17, %for_body_0]
  %11 = icmp ule i16 %10, %9
  br i1 %11, label %for_body_0, label %for_end_0
for_body_0:
  %12 = load i64, i64* %7
  %13 = getelementptr %inner_node_t_1, %inner_node_t_1* %6, i32 0, i32 1, i16 %10
  %14 = load %node_t_1*, %node_t_1** %13
  %15 = call ccc i64 @eclair_btree_node_count_entries_1(%node_t_1* %14)
  %16 = add i64 %12, %15
  store i64 %16, i64* %7
  %17 = add i16 1, %10
  br label %for_begin_0
for_end_0:
  %18 = load i64, i64* %7
  ret i64 %18
}

define external ccc void @eclair_btree_iterator_init_1(%btree_iterator_t_1* %iter_0, %node_t_1* %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 0
  store %node_t_1* %cur_0, %node_t_1** %0
  %1 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 1
  store i16 %pos_0, i16* %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_1(%btree_iterator_t_1* %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_1(%btree_iterator_t_1* %iter_0, %node_t_1* zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_1(%btree_iterator_t_1* %lhs_0, %btree_iterator_t_1* %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %lhs_0, i32 0, i32 0
  %1 = load %node_t_1*, %node_t_1** %0
  %2 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %rhs_0, i32 0, i32 0
  %3 = load %node_t_1*, %node_t_1** %2
  %4 = icmp ne %node_t_1* %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %lhs_0, i32 0, i32 1
  %6 = load i16, i16* %5
  %7 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %rhs_0, i32 0, i32 1
  %8 = load i16, i16* %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc [2 x i32]* @eclair_btree_iterator_current_1(%btree_iterator_t_1* %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 1
  %1 = load i16, i16* %0
  %2 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 0
  %3 = load %node_t_1*, %node_t_1** %2
  %4 = getelementptr %node_t_1, %node_t_1* %3, i32 0, i32 1, i16 %1
  ret [2 x i32]* %4
}

define external ccc void @eclair_btree_iterator_next_1(%btree_iterator_t_1* %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 0
  %1 = load %node_t_1*, %node_t_1** %0
  %2 = getelementptr %node_t_1, %node_t_1* %1, i32 0, i32 0, i32 3
  %3 = load i1, i1* %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 1
  %6 = load i16, i16* %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 0
  %9 = load %node_t_1*, %node_t_1** %8
  %10 = bitcast %node_t_1* %9 to %inner_node_t_1*
  %11 = getelementptr %inner_node_t_1, %inner_node_t_1* %10, i32 0, i32 1, i16 %7
  %12 = load %node_t_1*, %node_t_1** %11
  %13 = alloca %node_t_1*
  store %node_t_1* %12, %node_t_1** %13
  br label %while_begin_0
while_begin_0:
  %14 = load %node_t_1*, %node_t_1** %13
  %15 = getelementptr %node_t_1, %node_t_1* %14, i32 0, i32 0, i32 3
  %16 = load i1, i1* %15
  %17 = icmp eq i1 %16, 1
  br i1 %17, label %while_body_0, label %while_end_0
while_body_0:
  %18 = load %node_t_1*, %node_t_1** %13
  %19 = bitcast %node_t_1* %18 to %inner_node_t_1*
  %20 = getelementptr %inner_node_t_1, %inner_node_t_1* %19, i32 0, i32 1, i16 0
  %21 = load %node_t_1*, %node_t_1** %20
  store %node_t_1* %21, %node_t_1** %13
  br label %while_begin_0
while_end_0:
  %22 = load %node_t_1*, %node_t_1** %13
  %23 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 0
  store %node_t_1* %22, %node_t_1** %23
  %24 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 1
  store i16 0, i16* %24
  %25 = getelementptr %node_t_1, %node_t_1* %22, i32 0, i32 0, i32 2
  %26 = load i16, i16* %25
  %27 = icmp ne i16 %26, 0
  br i1 %27, label %if_1, label %end_if_0
if_1:
  ret void
end_if_0:
  br label %leaf.next_0
end_if_1:
  br label %leaf.next_0
leaf.next_0:
  %28 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 1
  %29 = load i16, i16* %28
  %30 = add i16 1, %29
  store i16 %30, i16* %28
  %31 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 1
  %32 = load i16, i16* %31
  %33 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 0
  %34 = load %node_t_1*, %node_t_1** %33
  %35 = getelementptr %node_t_1, %node_t_1* %34, i32 0, i32 0, i32 2
  %36 = load i16, i16* %35
  %37 = icmp ult i16 %32, %36
  br i1 %37, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %38 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 0
  %39 = load %node_t_1*, %node_t_1** %38
  %40 = icmp eq %node_t_1* %39, zeroinitializer
  br i1 %40, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %41 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 1
  %42 = load i16, i16* %41
  %43 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 0
  %44 = load %node_t_1*, %node_t_1** %43
  %45 = getelementptr %node_t_1, %node_t_1* %44, i32 0, i32 0, i32 2
  %46 = load i16, i16* %45
  %47 = icmp eq i16 %42, %46
  br label %loop.condition.end_0
loop.condition.end_0:
  %48 = phi i1 [0, %leaf.no_parent_0], [%47, %leaf.has_parent_0]
  br i1 %48, label %while_body_1, label %while_end_1
while_body_1:
  %49 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 0
  %50 = load %node_t_1*, %node_t_1** %49
  %51 = getelementptr %node_t_1, %node_t_1* %50, i32 0, i32 0, i32 1
  %52 = load i16, i16* %51
  %53 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 1
  store i16 %52, i16* %53
  %54 = getelementptr %node_t_1, %node_t_1* %50, i32 0, i32 0, i32 0
  %55 = load %node_t_1*, %node_t_1** %54
  %56 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %iter_0, i32 0, i32 0
  store %node_t_1* %55, %node_t_1** %56
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc [2 x i32]* @eclair_btree_linear_search_lower_bound_1([2 x i32]* %val_0, [2 x i32]* %current_0, [2 x i32]* %end_0) {
start:
  %0 = alloca [2 x i32]*
  store [2 x i32]* %current_0, [2 x i32]** %0
  br label %while_begin_0
while_begin_0:
  %1 = load [2 x i32]*, [2 x i32]** %0
  %2 = icmp ne [2 x i32]* %1, %end_0
  br i1 %2, label %while_body_0, label %while_end_0
while_body_0:
  %3 = load [2 x i32]*, [2 x i32]** %0
  %4 = call ccc i8 @eclair_btree_value_compare_values_1([2 x i32]* %3, [2 x i32]* %val_0)
  %5 = icmp ne i8 %4, -1
  br i1 %5, label %if_0, label %end_if_0
if_0:
  ret [2 x i32]* %3
end_if_0:
  %6 = getelementptr [2 x i32], [2 x i32]* %3, i32 1
  store [2 x i32]* %6, [2 x i32]** %0
  br label %while_begin_0
while_end_0:
  ret [2 x i32]* %end_0
}

define external ccc [2 x i32]* @eclair_btree_linear_search_upper_bound_1([2 x i32]* %val_0, [2 x i32]* %current_0, [2 x i32]* %end_0) {
start:
  %0 = alloca [2 x i32]*
  store [2 x i32]* %current_0, [2 x i32]** %0
  br label %while_begin_0
while_begin_0:
  %1 = load [2 x i32]*, [2 x i32]** %0
  %2 = icmp ne [2 x i32]* %1, %end_0
  br i1 %2, label %while_body_0, label %while_end_0
while_body_0:
  %3 = load [2 x i32]*, [2 x i32]** %0
  %4 = call ccc i8 @eclair_btree_value_compare_values_1([2 x i32]* %3, [2 x i32]* %val_0)
  %5 = icmp eq i8 %4, 1
  br i1 %5, label %if_0, label %end_if_0
if_0:
  ret [2 x i32]* %3
end_if_0:
  %6 = getelementptr [2 x i32], [2 x i32]* %3, i32 1
  store [2 x i32]* %6, [2 x i32]** %0
  br label %while_begin_0
while_end_0:
  ret [2 x i32]* %end_0
}

define external ccc void @eclair_btree_init_empty_1(%btree_t_1* %tree_0) {
start:
  %0 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 0
  store %node_t_1* zeroinitializer, %node_t_1** %0
  %1 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 1
  store %node_t_1* zeroinitializer, %node_t_1** %1
  ret void
}

define external ccc void @eclair_btree_init_1(%btree_t_1* %tree_0, %btree_iterator_t_1* %start_0, %btree_iterator_t_1* %end_0) {
start:
  call ccc void @eclair_btree_insert_range__1(%btree_t_1* %tree_0, %btree_iterator_t_1* %start_0, %btree_iterator_t_1* %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_1(%btree_t_1* %tree_0) {
start:
  call ccc void @eclair_btree_clear_1(%btree_t_1* %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_1(%btree_t_1* %tree_0) {
start:
  %0 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 0
  %1 = load %node_t_1*, %node_t_1** %0
  %2 = icmp eq %node_t_1* %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_1(%btree_t_1* %tree_0) {
start:
  %0 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 0
  %1 = load %node_t_1*, %node_t_1** %0
  %2 = icmp eq %node_t_1* %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_1(%node_t_1* %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_1() {
start:
  %0 = mul i16 3, 30
  %1 = udiv i16 %0, 4
  %2 = sub i16 30, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_1(%node_t_1* %node_0, %node_t_1** %root_0) {
start:
  %0 = call ccc i16 @eclair_btree_node_split_point_1()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 3
  %3 = load i1, i1* %2
  %4 = call ccc %node_t_1* @eclair_btree_node_new_1(i1 %3)
  %5 = alloca i16
  store i16 0, i16* %5
  br label %for_begin_0
for_begin_0:
  %6 = phi i16 [%1, %start], [%13, %for_body_0]
  %7 = icmp ult i16 %6, 30
  br i1 %7, label %for_body_0, label %for_end_0
for_body_0:
  %8 = load i16, i16* %5
  %9 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 1, i16 %6
  %10 = load [2 x i32], [2 x i32]* %9
  %11 = getelementptr %node_t_1, %node_t_1* %4, i32 0, i32 1, i16 %8
  store [2 x i32] %10, [2 x i32]* %11
  %12 = add i16 1, %8
  store i16 %12, i16* %5
  %13 = add i16 1, %6
  br label %for_begin_0
for_end_0:
  %14 = icmp eq i1 %3, 1
  br i1 %14, label %if_0, label %end_if_0
if_0:
  %15 = bitcast %node_t_1* %4 to %inner_node_t_1*
  %16 = bitcast %node_t_1* %node_0 to %inner_node_t_1*
  store i16 0, i16* %5
  br label %for_begin_1
for_begin_1:
  %17 = phi i16 [%1, %if_0], [%26, %for_body_1]
  %18 = icmp ule i16 %17, 30
  br i1 %18, label %for_body_1, label %for_end_1
for_body_1:
  %19 = load i16, i16* %5
  %20 = getelementptr %inner_node_t_1, %inner_node_t_1* %16, i32 0, i32 1, i16 %17
  %21 = load %node_t_1*, %node_t_1** %20
  %22 = getelementptr %node_t_1, %node_t_1* %21, i32 0, i32 0, i32 0
  store %node_t_1* %4, %node_t_1** %22
  %23 = getelementptr %node_t_1, %node_t_1* %21, i32 0, i32 0, i32 1
  store i16 %19, i16* %23
  %24 = getelementptr %inner_node_t_1, %inner_node_t_1* %15, i32 0, i32 1, i16 %19
  store %node_t_1* %21, %node_t_1** %24
  %25 = add i16 1, %19
  store i16 %25, i16* %5
  %26 = add i16 1, %17
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %27 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 2
  store i16 %0, i16* %27
  %28 = sub i16 30, %0
  %29 = sub i16 %28, 1
  %30 = getelementptr %node_t_1, %node_t_1* %4, i32 0, i32 0, i32 2
  store i16 %29, i16* %30
  call ccc void @eclair_btree_node_grow_parent_1(%node_t_1* %node_0, %node_t_1** %root_0, %node_t_1* %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_1(%node_t_1* %node_0, %node_t_1** %root_0, %node_t_1* %sibling_0) {
start:
  %0 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 0
  %1 = load %node_t_1*, %node_t_1** %0
  %2 = icmp eq %node_t_1* %1, zeroinitializer
  %3 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 2
  %4 = load i16, i16* %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc %node_t_1* @eclair_btree_node_new_1(i1 1)
  %6 = bitcast %node_t_1* %5 to %inner_node_t_1*
  %7 = getelementptr %node_t_1, %node_t_1* %5, i32 0, i32 0, i32 2
  store i16 1, i16* %7
  %8 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 1, i16 %4
  %9 = load [2 x i32], [2 x i32]* %8
  %10 = getelementptr %node_t_1, %node_t_1* %5, i32 0, i32 1, i16 0
  store [2 x i32] %9, [2 x i32]* %10
  %11 = getelementptr %inner_node_t_1, %inner_node_t_1* %6, i32 0, i32 1, i16 0
  store %node_t_1* %node_0, %node_t_1** %11
  %12 = getelementptr %inner_node_t_1, %inner_node_t_1* %6, i32 0, i32 1, i16 1
  store %node_t_1* %sibling_0, %node_t_1** %12
  %13 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 0
  store %node_t_1* %5, %node_t_1** %13
  %14 = getelementptr %node_t_1, %node_t_1* %sibling_0, i32 0, i32 0, i32 0
  store %node_t_1* %5, %node_t_1** %14
  %15 = getelementptr %node_t_1, %node_t_1* %sibling_0, i32 0, i32 0, i32 1
  store i16 1, i16* %15
  store %node_t_1* %5, %node_t_1** %root_0
  ret void
insert_new_node_in_parent_0:
  %16 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 1
  %17 = load i16, i16* %16
  %18 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_1(%node_t_1* %1, %node_t_1** %root_0, i16 %17, %node_t_1* %node_0, [2 x i32]* %18, %node_t_1* %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_1(%node_t_1* %node_0, %node_t_1** %root_0, i16 %pos_0, %node_t_1* %predecessor_0, [2 x i32]* %key_0, %node_t_1* %new_node_0) {
start:
  %0 = alloca i16
  store i16 %pos_0, i16* %0
  %1 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 2
  %2 = load i16, i16* %1
  %3 = icmp uge i16 %2, 30
  br i1 %3, label %if_0, label %end_if_1
if_0:
  %4 = load i16, i16* %0
  %5 = call ccc i16 @eclair_btree_node_rebalance_or_split_1(%node_t_1* %node_0, %node_t_1** %root_0, i16 %pos_0)
  %6 = sub i16 %4, %5
  store i16 %6, i16* %0
  %7 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 2
  %8 = load i16, i16* %7
  %9 = icmp ugt i16 %6, %8
  br i1 %9, label %if_1, label %end_if_0
if_1:
  %10 = sub i16 %6, %8
  %11 = sub i16 %10, 1
  store i16 %11, i16* %0
  %12 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 0
  %13 = load %node_t_1*, %node_t_1** %12
  %14 = bitcast %node_t_1* %13 to %inner_node_t_1*
  %15 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 1
  %16 = load i16, i16* %15
  %17 = add i16 1, %16
  %18 = getelementptr %inner_node_t_1, %inner_node_t_1* %14, i32 0, i32 1, i16 %17
  %19 = load %node_t_1*, %node_t_1** %18
  call ccc void @eclair_btree_node_insert_inner_1(%node_t_1* %19, %node_t_1** %root_0, i16 %11, %node_t_1* %predecessor_0, [2 x i32]* %key_0, %node_t_1* %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %20 = bitcast %node_t_1* %node_0 to %inner_node_t_1*
  %21 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 2
  %22 = load i16, i16* %21
  %23 = sub i16 %22, 1
  %24 = load i16, i16* %0
  br label %for_begin_0
for_begin_0:
  %25 = phi i16 [%23, %end_if_1], [%40, %for_body_0]
  %26 = icmp uge i16 %25, %24
  br i1 %26, label %for_body_0, label %for_end_0
for_body_0:
  %27 = add i16 %25, 1
  %28 = add i16 %25, 2
  %29 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 1, i16 %25
  %30 = load [2 x i32], [2 x i32]* %29
  %31 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 1, i16 %27
  store [2 x i32] %30, [2 x i32]* %31
  %32 = getelementptr %inner_node_t_1, %inner_node_t_1* %20, i32 0, i32 1, i16 %27
  %33 = load %node_t_1*, %node_t_1** %32
  %34 = getelementptr %inner_node_t_1, %inner_node_t_1* %20, i32 0, i32 1, i16 %28
  store %node_t_1* %33, %node_t_1** %34
  %35 = getelementptr %inner_node_t_1, %inner_node_t_1* %20, i32 0, i32 1, i16 %28
  %36 = load %node_t_1*, %node_t_1** %35
  %37 = getelementptr %node_t_1, %node_t_1* %36, i32 0, i32 0, i32 1
  %38 = load i16, i16* %37
  %39 = add i16 1, %38
  store i16 %39, i16* %37
  %40 = sub i16 %25, 1
  br label %for_begin_0
for_end_0:
  %41 = load [2 x i32], [2 x i32]* %key_0
  %42 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 1, i16 %24
  store [2 x i32] %41, [2 x i32]* %42
  %43 = add i16 %24, 1
  %44 = getelementptr %inner_node_t_1, %inner_node_t_1* %20, i32 0, i32 1, i16 %43
  store %node_t_1* %new_node_0, %node_t_1** %44
  %45 = getelementptr %node_t_1, %node_t_1* %new_node_0, i32 0, i32 0, i32 0
  store %node_t_1* %node_0, %node_t_1** %45
  %46 = getelementptr %node_t_1, %node_t_1* %new_node_0, i32 0, i32 0, i32 1
  store i16 %43, i16* %46
  %47 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 2
  %48 = load i16, i16* %47
  %49 = add i16 1, %48
  store i16 %49, i16* %47
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_1(%node_t_1* %node_0, %node_t_1** %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 0
  %1 = load %node_t_1*, %node_t_1** %0
  %2 = bitcast %node_t_1* %1 to %inner_node_t_1*
  %3 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 1
  %4 = load i16, i16* %3
  %5 = icmp ne %inner_node_t_1* %2, zeroinitializer
  %6 = icmp ugt i16 %4, 0
  %7 = and i1 %5, %6
  br i1 %7, label %rebalance_0, label %split_0
rebalance_0:
  %8 = sub i16 %4, 1
  %9 = getelementptr %inner_node_t_1, %inner_node_t_1* %2, i32 0, i32 1, i16 %8
  %10 = load %node_t_1*, %node_t_1** %9
  %11 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 0, i32 2
  %12 = load i16, i16* %11
  %13 = sub i16 30, %12
  %14 = icmp slt i16 %13, %idx_0
  %15 = select i1 %14, i16 %13, i16 %idx_0
  %16 = icmp ugt i16 %15, 0
  br i1 %16, label %if_0, label %end_if_1
if_0:
  %17 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 1
  %18 = load i16, i16* %17
  %19 = sub i16 %18, 1
  %20 = getelementptr %inner_node_t_1, %inner_node_t_1* %2, i32 0, i32 0, i32 1, i16 %19
  %21 = load [2 x i32], [2 x i32]* %20
  %22 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 0, i32 2
  %23 = load i16, i16* %22
  %24 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 1, i16 %23
  store [2 x i32] %21, [2 x i32]* %24
  %25 = sub i16 %15, 1
  br label %for_begin_0
for_begin_0:
  %26 = phi i16 [0, %if_0], [%33, %for_body_0]
  %27 = icmp ult i16 %26, %25
  br i1 %27, label %for_body_0, label %for_end_0
for_body_0:
  %28 = add i16 %23, 1
  %29 = add i16 %26, %28
  %30 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 1, i16 %26
  %31 = load [2 x i32], [2 x i32]* %30
  %32 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 1, i16 %29
  store [2 x i32] %31, [2 x i32]* %32
  %33 = add i16 1, %26
  br label %for_begin_0
for_end_0:
  %34 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 1, i16 %25
  %35 = load [2 x i32], [2 x i32]* %34
  store [2 x i32] %35, [2 x i32]* %20
  %36 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 2
  %37 = load i16, i16* %36
  %38 = sub i16 %37, %15
  br label %for_begin_1
for_begin_1:
  %39 = phi i16 [0, %for_end_0], [%45, %for_body_1]
  %40 = icmp ult i16 %39, %38
  br i1 %40, label %for_body_1, label %for_end_1
for_body_1:
  %41 = add i16 %39, %15
  %42 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 1, i16 %41
  %43 = load [2 x i32], [2 x i32]* %42
  %44 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 1, i16 %39
  store [2 x i32] %43, [2 x i32]* %44
  %45 = add i16 1, %39
  br label %for_begin_1
for_end_1:
  %46 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 3
  %47 = load i1, i1* %46
  %48 = icmp eq i1 %47, 1
  br i1 %48, label %if_1, label %end_if_0
if_1:
  %49 = bitcast %node_t_1* %node_0 to %inner_node_t_1*
  %50 = bitcast %node_t_1* %10 to %inner_node_t_1*
  br label %for_begin_2
for_begin_2:
  %51 = phi i16 [0, %if_1], [%64, %for_body_2]
  %52 = icmp ult i16 %51, %15
  br i1 %52, label %for_body_2, label %for_end_2
for_body_2:
  %53 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 0, i32 2
  %54 = load i16, i16* %53
  %55 = add i16 %54, 1
  %56 = add i16 %51, %55
  %57 = getelementptr %inner_node_t_1, %inner_node_t_1* %49, i32 0, i32 1, i16 %51
  %58 = load %node_t_1*, %node_t_1** %57
  %59 = getelementptr %inner_node_t_1, %inner_node_t_1* %50, i32 0, i32 1, i16 %56
  store %node_t_1* %58, %node_t_1** %59
  %60 = getelementptr %inner_node_t_1, %inner_node_t_1* %50, i32 0, i32 1, i16 %56
  %61 = load %node_t_1*, %node_t_1** %60
  %62 = getelementptr %node_t_1, %node_t_1* %61, i32 0, i32 0, i32 0
  store %node_t_1* %10, %node_t_1** %62
  %63 = getelementptr %node_t_1, %node_t_1* %61, i32 0, i32 0, i32 1
  store i16 %56, i16* %63
  %64 = add i16 1, %51
  br label %for_begin_2
for_end_2:
  %65 = sub i16 %37, %15
  %66 = add i16 1, %65
  br label %for_begin_3
for_begin_3:
  %67 = phi i16 [0, %for_end_2], [%76, %for_body_3]
  %68 = icmp ult i16 %67, %66
  br i1 %68, label %for_body_3, label %for_end_3
for_body_3:
  %69 = add i16 %67, %15
  %70 = getelementptr %inner_node_t_1, %inner_node_t_1* %49, i32 0, i32 1, i16 %69
  %71 = load %node_t_1*, %node_t_1** %70
  %72 = getelementptr %inner_node_t_1, %inner_node_t_1* %49, i32 0, i32 1, i16 %67
  store %node_t_1* %71, %node_t_1** %72
  %73 = getelementptr %inner_node_t_1, %inner_node_t_1* %49, i32 0, i32 1, i16 %67
  %74 = load %node_t_1*, %node_t_1** %73
  %75 = getelementptr %node_t_1, %node_t_1* %74, i32 0, i32 0, i32 1
  store i16 %67, i16* %75
  %76 = add i16 1, %67
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %77 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 0, i32 2
  %78 = load i16, i16* %77
  %79 = add i16 %78, %15
  store i16 %79, i16* %77
  %80 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 2
  %81 = load i16, i16* %80
  %82 = sub i16 %81, %15
  store i16 %82, i16* %80
  ret i16 %15
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_1(%node_t_1* %node_0, %node_t_1** %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %tree_0, [2 x i32]* %val_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_1(%btree_t_1* %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc %node_t_1* @eclair_btree_node_new_1(i1 0)
  %2 = getelementptr %node_t_1, %node_t_1* %1, i32 0, i32 0, i32 2
  store i16 1, i16* %2
  %3 = load [2 x i32], [2 x i32]* %val_0
  %4 = getelementptr %node_t_1, %node_t_1* %1, i32 0, i32 1, i16 0
  store [2 x i32] %3, [2 x i32]* %4
  %5 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 0
  store %node_t_1* %1, %node_t_1** %5
  %6 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 1
  store %node_t_1* %1, %node_t_1** %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 0
  %8 = load %node_t_1*, %node_t_1** %7
  %9 = alloca %node_t_1*
  store %node_t_1* %8, %node_t_1** %9
  br label %loop_0
loop_0:
  %10 = load %node_t_1*, %node_t_1** %9
  %11 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 0, i32 3
  %12 = load i1, i1* %11
  %13 = icmp eq i1 %12, 1
  br i1 %13, label %inner_0, label %leaf_0
inner_0:
  %14 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 0, i32 2
  %15 = load i16, i16* %14
  %16 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 1, i16 0
  %17 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 1, i16 %15
  %18 = call ccc [2 x i32]* @eclair_btree_linear_search_lower_bound_1([2 x i32]* %val_0, [2 x i32]* %16, [2 x i32]* %17)
  %19 = ptrtoint [2 x i32]* %18 to i64
  %20 = ptrtoint [2 x i32]* %16 to i64
  %21 = sub i64 %19, %20
  %22 = trunc i64 %21 to i16
  %23 = udiv i16 %22, 8
  %24 = icmp ne [2 x i32]* %18, %17
  %25 = call ccc i8 @eclair_btree_value_compare_values_1([2 x i32]* %18, [2 x i32]* %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %28 = bitcast %node_t_1* %10 to %inner_node_t_1*
  %29 = getelementptr %inner_node_t_1, %inner_node_t_1* %28, i32 0, i32 1, i16 %23
  %30 = load %node_t_1*, %node_t_1** %29
  store %node_t_1* %30, %node_t_1** %9
  br label %loop_0
leaf_0:
  %31 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 0, i32 2
  %32 = load i16, i16* %31
  %33 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 1, i16 0
  %34 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 1, i16 %32
  %35 = call ccc [2 x i32]* @eclair_btree_linear_search_upper_bound_1([2 x i32]* %val_0, [2 x i32]* %33, [2 x i32]* %34)
  %36 = ptrtoint [2 x i32]* %35 to i64
  %37 = ptrtoint [2 x i32]* %33 to i64
  %38 = sub i64 %36, %37
  %39 = trunc i64 %38 to i16
  %40 = udiv i16 %39, 8
  %41 = alloca i16
  store i16 %40, i16* %41
  %42 = icmp ne [2 x i32]* %35, %33
  %43 = getelementptr [2 x i32], [2 x i32]* %35, i32 -1
  %44 = call ccc i8 @eclair_btree_value_compare_values_1([2 x i32]* %43, [2 x i32]* %val_0)
  %45 = icmp eq i8 0, %44
  %46 = and i1 %42, %45
  br i1 %46, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %47 = icmp uge i16 %32, 30
  br i1 %47, label %split_0, label %no_split_0
split_0:
  %48 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 0
  %49 = load i16, i16* %41
  %50 = call ccc i16 @eclair_btree_node_rebalance_or_split_1(%node_t_1* %10, %node_t_1** %48, i16 %49)
  %51 = sub i16 %49, %50
  store i16 %51, i16* %41
  %52 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 0, i32 2
  %53 = load i16, i16* %52
  %54 = icmp ugt i16 %51, %53
  br i1 %54, label %if_0, label %end_if_0
if_0:
  %55 = add i16 %53, 1
  %56 = sub i16 %51, %55
  store i16 %56, i16* %41
  %57 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 0, i32 0
  %58 = load %node_t_1*, %node_t_1** %57
  %59 = bitcast %node_t_1* %58 to %inner_node_t_1*
  %60 = getelementptr %node_t_1, %node_t_1* %10, i32 0, i32 0, i32 1
  %61 = load i16, i16* %60
  %62 = add i16 1, %61
  %63 = getelementptr %inner_node_t_1, %inner_node_t_1* %59, i32 0, i32 1, i16 %62
  %64 = load %node_t_1*, %node_t_1** %63
  store %node_t_1* %64, %node_t_1** %9
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %65 = load %node_t_1*, %node_t_1** %9
  %66 = load i16, i16* %41
  %67 = getelementptr %node_t_1, %node_t_1* %65, i32 0, i32 0, i32 2
  %68 = load i16, i16* %67
  br label %for_begin_0
for_begin_0:
  %69 = phi i16 [%68, %no_split_0], [%75, %for_body_0]
  %70 = icmp ugt i16 %69, %66
  br i1 %70, label %for_body_0, label %for_end_0
for_body_0:
  %71 = sub i16 %69, 1
  %72 = getelementptr %node_t_1, %node_t_1* %65, i32 0, i32 1, i16 %71
  %73 = load [2 x i32], [2 x i32]* %72
  %74 = getelementptr %node_t_1, %node_t_1* %65, i32 0, i32 1, i16 %69
  store [2 x i32] %73, [2 x i32]* %74
  %75 = sub i16 %69, 1
  br label %for_begin_0
for_end_0:
  %76 = load [2 x i32], [2 x i32]* %val_0
  %77 = getelementptr %node_t_1, %node_t_1* %65, i32 0, i32 1, i16 %66
  store [2 x i32] %76, [2 x i32]* %77
  %78 = getelementptr %node_t_1, %node_t_1* %65, i32 0, i32 0, i32 2
  %79 = load i16, i16* %78
  %80 = add i16 1, %79
  store i16 %80, i16* %78
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__1(%btree_t_1* %tree_0, %btree_iterator_t_1* %begin_0, %btree_iterator_t_1* %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_1(%btree_iterator_t_1* %begin_0, %btree_iterator_t_1* %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc [2 x i32]* @eclair_btree_iterator_current_1(%btree_iterator_t_1* %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %tree_0, [2 x i32]* %2)
  call ccc void @eclair_btree_iterator_next_1(%btree_iterator_t_1* %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_1(%btree_t_1* %tree_0, %btree_iterator_t_1* %result_0) {
start:
  %0 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 1
  %1 = load %node_t_1*, %node_t_1** %0
  %2 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %result_0, i32 0, i32 0
  store %node_t_1* %1, %node_t_1** %2
  %3 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %result_0, i32 0, i32 1
  store i16 0, i16* %3
  ret void
}

define external ccc void @eclair_btree_end_1(%btree_t_1* %tree_0, %btree_iterator_t_1* %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_1(%btree_iterator_t_1* %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_1(%btree_t_1* %tree_0, [2 x i32]* %val_0) {
start:
  %0 = alloca %btree_iterator_t_1, i32 1
  %1 = alloca %btree_iterator_t_1, i32 1
  call ccc void @eclair_btree_find_1(%btree_t_1* %tree_0, [2 x i32]* %val_0, %btree_iterator_t_1* %0)
  call ccc void @eclair_btree_end_1(%btree_t_1* %tree_0, %btree_iterator_t_1* %1)
  %2 = call ccc i1 @eclair_btree_iterator_is_equal_1(%btree_iterator_t_1* %0, %btree_iterator_t_1* %1)
  %3 = select i1 %2, i1 0, i1 1
  ret i1 %3
}

define external ccc void @eclair_btree_find_1(%btree_t_1* %tree_0, [2 x i32]* %val_0, %btree_iterator_t_1* %result_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_1(%btree_t_1* %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_1(%btree_iterator_t_1* %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 0
  %2 = load %node_t_1*, %node_t_1** %1
  %3 = alloca %node_t_1*
  store %node_t_1* %2, %node_t_1** %3
  br label %loop_0
loop_0:
  %4 = load %node_t_1*, %node_t_1** %3
  %5 = getelementptr %node_t_1, %node_t_1* %4, i32 0, i32 0, i32 2
  %6 = load i16, i16* %5
  %7 = getelementptr %node_t_1, %node_t_1* %4, i32 0, i32 1, i16 0
  %8 = getelementptr %node_t_1, %node_t_1* %4, i32 0, i32 1, i16 %6
  %9 = call ccc [2 x i32]* @eclair_btree_linear_search_lower_bound_1([2 x i32]* %val_0, [2 x i32]* %7, [2 x i32]* %8)
  %10 = ptrtoint [2 x i32]* %9 to i64
  %11 = ptrtoint [2 x i32]* %7 to i64
  %12 = sub i64 %10, %11
  %13 = trunc i64 %12 to i16
  %14 = udiv i16 %13, 8
  %15 = icmp ult [2 x i32]* %9, %8
  %16 = call ccc i8 @eclair_btree_value_compare_values_1([2 x i32]* %9, [2 x i32]* %val_0)
  %17 = icmp eq i8 0, %16
  %18 = and i1 %15, %17
  br i1 %18, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_1(%btree_iterator_t_1* %result_0, %node_t_1* %4, i16 %14)
  ret void
end_if_1:
  %19 = getelementptr %node_t_1, %node_t_1* %4, i32 0, i32 0, i32 3
  %20 = load i1, i1* %19
  %21 = icmp eq i1 %20, 0
  br i1 %21, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_1(%btree_iterator_t_1* %result_0)
  ret void
end_if_2:
  %22 = bitcast %node_t_1* %4 to %inner_node_t_1*
  %23 = getelementptr %inner_node_t_1, %inner_node_t_1* %22, i32 0, i32 1, i16 %14
  %24 = load %node_t_1*, %node_t_1** %23
  store %node_t_1* %24, %node_t_1** %3
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_1(%btree_t_1* %tree_0, [2 x i32]* %val_0, %btree_iterator_t_1* %result_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_1(%btree_t_1* %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_1(%btree_iterator_t_1* %result_0)
  ret void
end_if_0:
  %1 = alloca %btree_iterator_t_1, i32 1
  call ccc void @eclair_btree_iterator_end_init_1(%btree_iterator_t_1* %1)
  %2 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 0
  %3 = load %node_t_1*, %node_t_1** %2
  %4 = alloca %node_t_1*
  store %node_t_1* %3, %node_t_1** %4
  br label %loop_0
loop_0:
  %5 = load %node_t_1*, %node_t_1** %4
  %6 = getelementptr %node_t_1, %node_t_1* %5, i32 0, i32 0, i32 2
  %7 = load i16, i16* %6
  %8 = getelementptr %node_t_1, %node_t_1* %5, i32 0, i32 1, i16 0
  %9 = getelementptr %node_t_1, %node_t_1* %5, i32 0, i32 1, i16 %7
  %10 = call ccc [2 x i32]* @eclair_btree_linear_search_lower_bound_1([2 x i32]* %val_0, [2 x i32]* %8, [2 x i32]* %9)
  %11 = ptrtoint [2 x i32]* %10 to i64
  %12 = ptrtoint [2 x i32]* %8 to i64
  %13 = sub i64 %11, %12
  %14 = trunc i64 %13 to i16
  %15 = udiv i16 %14, 8
  %16 = getelementptr %node_t_1, %node_t_1* %5, i32 0, i32 0, i32 3
  %17 = load i1, i1* %16
  %18 = icmp eq i1 %17, 0
  br i1 %18, label %if_1, label %end_if_1
if_1:
  %19 = icmp eq [2 x i32]* %10, %9
  br i1 %19, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %20 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %1, i32 0, i32 0
  %21 = load %node_t_1*, %node_t_1** %20
  %22 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %result_0, i32 0, i32 0
  store %node_t_1* %21, %node_t_1** %22
  %23 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %1, i32 0, i32 1
  %24 = load i16, i16* %23
  %25 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %result_0, i32 0, i32 1
  store i16 %24, i16* %25
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_1(%btree_iterator_t_1* %result_0, %node_t_1* %5, i16 %15)
  ret void
end_if_1:
  %26 = icmp ne [2 x i32]* %10, %9
  %27 = call ccc i8 @eclair_btree_value_compare_values_1([2 x i32]* %10, [2 x i32]* %val_0)
  %28 = icmp eq i8 0, %27
  %29 = and i1 %26, %28
  br i1 %29, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_1(%btree_iterator_t_1* %result_0, %node_t_1* %5, i16 %15)
  ret void
end_if_2:
  br i1 %26, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_1(%btree_iterator_t_1* %1, %node_t_1* %5, i16 %15)
  br label %end_if_3
end_if_3:
  %30 = bitcast %node_t_1* %5 to %inner_node_t_1*
  %31 = getelementptr %inner_node_t_1, %inner_node_t_1* %30, i32 0, i32 1, i16 %15
  %32 = load %node_t_1*, %node_t_1** %31
  store %node_t_1* %32, %node_t_1** %4
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_1(%btree_t_1* %tree_0, [2 x i32]* %val_0, %btree_iterator_t_1* %result_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_1(%btree_t_1* %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_1(%btree_iterator_t_1* %result_0)
  ret void
end_if_0:
  %1 = alloca %btree_iterator_t_1, i32 1
  call ccc void @eclair_btree_iterator_end_init_1(%btree_iterator_t_1* %1)
  %2 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 0
  %3 = load %node_t_1*, %node_t_1** %2
  %4 = alloca %node_t_1*
  store %node_t_1* %3, %node_t_1** %4
  br label %loop_0
loop_0:
  %5 = load %node_t_1*, %node_t_1** %4
  %6 = getelementptr %node_t_1, %node_t_1* %5, i32 0, i32 0, i32 2
  %7 = load i16, i16* %6
  %8 = getelementptr %node_t_1, %node_t_1* %5, i32 0, i32 1, i16 0
  %9 = getelementptr %node_t_1, %node_t_1* %5, i32 0, i32 1, i16 %7
  %10 = call ccc [2 x i32]* @eclair_btree_linear_search_upper_bound_1([2 x i32]* %val_0, [2 x i32]* %8, [2 x i32]* %9)
  %11 = ptrtoint [2 x i32]* %10 to i64
  %12 = ptrtoint [2 x i32]* %8 to i64
  %13 = sub i64 %11, %12
  %14 = trunc i64 %13 to i16
  %15 = udiv i16 %14, 8
  %16 = getelementptr %node_t_1, %node_t_1* %5, i32 0, i32 0, i32 3
  %17 = load i1, i1* %16
  %18 = icmp eq i1 %17, 0
  br i1 %18, label %if_1, label %end_if_1
if_1:
  %19 = icmp eq [2 x i32]* %10, %9
  br i1 %19, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %20 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %1, i32 0, i32 0
  %21 = load %node_t_1*, %node_t_1** %20
  %22 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %result_0, i32 0, i32 0
  store %node_t_1* %21, %node_t_1** %22
  %23 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %1, i32 0, i32 1
  %24 = load i16, i16* %23
  %25 = getelementptr %btree_iterator_t_1, %btree_iterator_t_1* %result_0, i32 0, i32 1
  store i16 %24, i16* %25
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_1(%btree_iterator_t_1* %result_0, %node_t_1* %5, i16 %15)
  ret void
end_if_1:
  %26 = icmp ne [2 x i32]* %10, %9
  br i1 %26, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_1(%btree_iterator_t_1* %result_0, %node_t_1* %5, i16 %15)
  br label %end_if_2
end_if_2:
  %27 = bitcast %node_t_1* %5 to %inner_node_t_1*
  %28 = getelementptr %inner_node_t_1, %inner_node_t_1* %27, i32 0, i32 1, i16 %15
  %29 = load %node_t_1*, %node_t_1** %28
  store %node_t_1* %29, %node_t_1** %4
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_1(%node_t_1* %node_0) {
start:
  %0 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 3
  %1 = load i1, i1* %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = bitcast %node_t_1* %node_0 to %inner_node_t_1*
  %4 = getelementptr %node_t_1, %node_t_1* %node_0, i32 0, i32 0, i32 2
  %5 = load i16, i16* %4
  br label %for_begin_0
for_begin_0:
  %6 = phi i16 [0, %if_0], [%11, %end_if_0]
  %7 = icmp ule i16 %6, %5
  br i1 %7, label %for_body_0, label %for_end_0
for_body_0:
  %8 = getelementptr %inner_node_t_1, %inner_node_t_1* %3, i32 0, i32 1, i16 %6
  %9 = load %node_t_1*, %node_t_1** %8
  %10 = icmp ne %node_t_1* %9, zeroinitializer
  br i1 %10, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_1(%node_t_1* %9)
  br label %end_if_0
end_if_0:
  %11 = add i16 1, %6
  br label %for_begin_0
for_end_0:
  br label %end_if_1
end_if_1:
  %12 = bitcast %node_t_1* %node_0 to i8*
  call ccc void @free(i8* %12)
  ret void
}

define external ccc void @eclair_btree_clear_1(%btree_t_1* %tree_0) {
start:
  %0 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 0
  %1 = load %node_t_1*, %node_t_1** %0
  %2 = icmp ne %node_t_1* %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_1(%node_t_1* %1)
  %3 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 0
  store %node_t_1* zeroinitializer, %node_t_1** %3
  %4 = getelementptr %btree_t_1, %btree_t_1* %tree_0, i32 0, i32 1
  store %node_t_1* zeroinitializer, %node_t_1** %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_1(%btree_t_1* %lhs_0, %btree_t_1* %rhs_0) {
start:
  %0 = getelementptr %btree_t_1, %btree_t_1* %lhs_0, i32 0, i32 0
  %1 = load %node_t_1*, %node_t_1** %0
  %2 = getelementptr %btree_t_1, %btree_t_1* %rhs_0, i32 0, i32 0
  %3 = load %node_t_1*, %node_t_1** %2
  %4 = getelementptr %btree_t_1, %btree_t_1* %lhs_0, i32 0, i32 0
  store %node_t_1* %3, %node_t_1** %4
  %5 = getelementptr %btree_t_1, %btree_t_1* %rhs_0, i32 0, i32 0
  store %node_t_1* %1, %node_t_1** %5
  %6 = getelementptr %btree_t_1, %btree_t_1* %lhs_0, i32 0, i32 1
  %7 = load %node_t_1*, %node_t_1** %6
  %8 = getelementptr %btree_t_1, %btree_t_1* %rhs_0, i32 0, i32 1
  %9 = load %node_t_1*, %node_t_1** %8
  %10 = getelementptr %btree_t_1, %btree_t_1* %lhs_0, i32 0, i32 1
  store %node_t_1* %9, %node_t_1** %10
  %11 = getelementptr %btree_t_1, %btree_t_1* %rhs_0, i32 0, i32 1
  store %node_t_1* %7, %node_t_1** %11
  ret void
}

%node_data_t_2 = type {%node_t_2*, i16, i16, i1}

%node_t_2 = type {%node_data_t_2, [60 x [1 x i32]]}

%inner_node_t_2 = type {%node_t_2, [61 x %node_t_2*]}

%btree_iterator_t_2 = type {%node_t_2*, i16}

%btree_t_2 = type {%node_t_2*, %node_t_2*}

define external ccc i8 @eclair_btree_value_compare_2(i32 %lhs_0, i32 %rhs_0) {
start:
  %0 = icmp ult i32 %lhs_0, %rhs_0
  br i1 %0, label %if_0, label %end_if_0
if_0:
  ret i8 -1
end_if_0:
  %1 = icmp ugt i32 %lhs_0, %rhs_0
  %2 = select i1 %1, i8 1, i8 0
  ret i8 %2
}

define external ccc i8 @eclair_btree_value_compare_values_2([1 x i32]* %lhs_0, [1 x i32]* %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [1 x i32], [1 x i32]* %lhs_0, i32 0, i32 0
  %1 = getelementptr [1 x i32], [1 x i32]* %rhs_0, i32 0, i32 0
  %2 = load i32, i32* %0
  %3 = load i32, i32* %1
  %4 = call ccc i8 @eclair_btree_value_compare_2(i32 %2, i32 %3)
  br label %end_0
end_0:
  %5 = phi i8 [%4, %comparison_0]
  ret i8 %5
}

define external ccc %node_t_2* @eclair_btree_node_new_2(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 744, i32 256
  %1 = call ccc i8* @malloc(i32 %0)
  %2 = bitcast i8* %1 to %node_t_2*
  %3 = getelementptr %node_t_2, %node_t_2* %2, i32 0, i32 0, i32 0
  store %node_t_2* zeroinitializer, %node_t_2** %3
  %4 = getelementptr %node_t_2, %node_t_2* %2, i32 0, i32 0, i32 1
  store i16 0, i16* %4
  %5 = getelementptr %node_t_2, %node_t_2* %2, i32 0, i32 0, i32 2
  store i16 0, i16* %5
  %6 = getelementptr %node_t_2, %node_t_2* %2, i32 0, i32 0, i32 3
  store i1 %type_0, i1* %6
  %7 = getelementptr %node_t_2, %node_t_2* %2, i32 0, i32 1
  %8 = bitcast [60 x [1 x i32]]* %7 to i8*
  call ccc void @llvm.memset.p0i8.i64(i8* %8, i8 0, i64 240, i1 0)
  %9 = icmp eq i1 %type_0, 1
  br i1 %9, label %if_0, label %end_if_0
if_0:
  %10 = bitcast %node_t_2* %2 to %inner_node_t_2*
  %11 = getelementptr %inner_node_t_2, %inner_node_t_2* %10, i32 0, i32 1
  %12 = bitcast [61 x %node_t_2*]* %11 to i8*
  call ccc void @llvm.memset.p0i8.i64(i8* %12, i8 0, i64 488, i1 0)
  br label %end_if_0
end_if_0:
  ret %node_t_2* %2
}

define external ccc i64 @eclair_btree_node_count_entries_2(%node_t_2* %node_0) {
start:
  %0 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 2
  %1 = load i16, i16* %0
  %2 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 3
  %3 = load i1, i1* %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  %6 = bitcast %node_t_2* %node_0 to %inner_node_t_2*
  %7 = alloca i64
  store i64 %5, i64* %7
  %8 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 2
  %9 = load i16, i16* %8
  br label %for_begin_0
for_begin_0:
  %10 = phi i16 [0, %end_if_0], [%17, %for_body_0]
  %11 = icmp ule i16 %10, %9
  br i1 %11, label %for_body_0, label %for_end_0
for_body_0:
  %12 = load i64, i64* %7
  %13 = getelementptr %inner_node_t_2, %inner_node_t_2* %6, i32 0, i32 1, i16 %10
  %14 = load %node_t_2*, %node_t_2** %13
  %15 = call ccc i64 @eclair_btree_node_count_entries_2(%node_t_2* %14)
  %16 = add i64 %12, %15
  store i64 %16, i64* %7
  %17 = add i16 1, %10
  br label %for_begin_0
for_end_0:
  %18 = load i64, i64* %7
  ret i64 %18
}

define external ccc void @eclair_btree_iterator_init_2(%btree_iterator_t_2* %iter_0, %node_t_2* %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 0
  store %node_t_2* %cur_0, %node_t_2** %0
  %1 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 1
  store i16 %pos_0, i16* %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_2(%btree_iterator_t_2* %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_2(%btree_iterator_t_2* %iter_0, %node_t_2* zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_2(%btree_iterator_t_2* %lhs_0, %btree_iterator_t_2* %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %lhs_0, i32 0, i32 0
  %1 = load %node_t_2*, %node_t_2** %0
  %2 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %rhs_0, i32 0, i32 0
  %3 = load %node_t_2*, %node_t_2** %2
  %4 = icmp ne %node_t_2* %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %lhs_0, i32 0, i32 1
  %6 = load i16, i16* %5
  %7 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %rhs_0, i32 0, i32 1
  %8 = load i16, i16* %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc [1 x i32]* @eclair_btree_iterator_current_2(%btree_iterator_t_2* %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 1
  %1 = load i16, i16* %0
  %2 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 0
  %3 = load %node_t_2*, %node_t_2** %2
  %4 = getelementptr %node_t_2, %node_t_2* %3, i32 0, i32 1, i16 %1
  ret [1 x i32]* %4
}

define external ccc void @eclair_btree_iterator_next_2(%btree_iterator_t_2* %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 0
  %1 = load %node_t_2*, %node_t_2** %0
  %2 = getelementptr %node_t_2, %node_t_2* %1, i32 0, i32 0, i32 3
  %3 = load i1, i1* %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 1
  %6 = load i16, i16* %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 0
  %9 = load %node_t_2*, %node_t_2** %8
  %10 = bitcast %node_t_2* %9 to %inner_node_t_2*
  %11 = getelementptr %inner_node_t_2, %inner_node_t_2* %10, i32 0, i32 1, i16 %7
  %12 = load %node_t_2*, %node_t_2** %11
  %13 = alloca %node_t_2*
  store %node_t_2* %12, %node_t_2** %13
  br label %while_begin_0
while_begin_0:
  %14 = load %node_t_2*, %node_t_2** %13
  %15 = getelementptr %node_t_2, %node_t_2* %14, i32 0, i32 0, i32 3
  %16 = load i1, i1* %15
  %17 = icmp eq i1 %16, 1
  br i1 %17, label %while_body_0, label %while_end_0
while_body_0:
  %18 = load %node_t_2*, %node_t_2** %13
  %19 = bitcast %node_t_2* %18 to %inner_node_t_2*
  %20 = getelementptr %inner_node_t_2, %inner_node_t_2* %19, i32 0, i32 1, i16 0
  %21 = load %node_t_2*, %node_t_2** %20
  store %node_t_2* %21, %node_t_2** %13
  br label %while_begin_0
while_end_0:
  %22 = load %node_t_2*, %node_t_2** %13
  %23 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 0
  store %node_t_2* %22, %node_t_2** %23
  %24 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 1
  store i16 0, i16* %24
  %25 = getelementptr %node_t_2, %node_t_2* %22, i32 0, i32 0, i32 2
  %26 = load i16, i16* %25
  %27 = icmp ne i16 %26, 0
  br i1 %27, label %if_1, label %end_if_0
if_1:
  ret void
end_if_0:
  br label %leaf.next_0
end_if_1:
  br label %leaf.next_0
leaf.next_0:
  %28 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 1
  %29 = load i16, i16* %28
  %30 = add i16 1, %29
  store i16 %30, i16* %28
  %31 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 1
  %32 = load i16, i16* %31
  %33 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 0
  %34 = load %node_t_2*, %node_t_2** %33
  %35 = getelementptr %node_t_2, %node_t_2* %34, i32 0, i32 0, i32 2
  %36 = load i16, i16* %35
  %37 = icmp ult i16 %32, %36
  br i1 %37, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %38 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 0
  %39 = load %node_t_2*, %node_t_2** %38
  %40 = icmp eq %node_t_2* %39, zeroinitializer
  br i1 %40, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %41 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 1
  %42 = load i16, i16* %41
  %43 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 0
  %44 = load %node_t_2*, %node_t_2** %43
  %45 = getelementptr %node_t_2, %node_t_2* %44, i32 0, i32 0, i32 2
  %46 = load i16, i16* %45
  %47 = icmp eq i16 %42, %46
  br label %loop.condition.end_0
loop.condition.end_0:
  %48 = phi i1 [0, %leaf.no_parent_0], [%47, %leaf.has_parent_0]
  br i1 %48, label %while_body_1, label %while_end_1
while_body_1:
  %49 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 0
  %50 = load %node_t_2*, %node_t_2** %49
  %51 = getelementptr %node_t_2, %node_t_2* %50, i32 0, i32 0, i32 1
  %52 = load i16, i16* %51
  %53 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 1
  store i16 %52, i16* %53
  %54 = getelementptr %node_t_2, %node_t_2* %50, i32 0, i32 0, i32 0
  %55 = load %node_t_2*, %node_t_2** %54
  %56 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %iter_0, i32 0, i32 0
  store %node_t_2* %55, %node_t_2** %56
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc [1 x i32]* @eclair_btree_linear_search_lower_bound_2([1 x i32]* %val_0, [1 x i32]* %current_0, [1 x i32]* %end_0) {
start:
  %0 = alloca [1 x i32]*
  store [1 x i32]* %current_0, [1 x i32]** %0
  br label %while_begin_0
while_begin_0:
  %1 = load [1 x i32]*, [1 x i32]** %0
  %2 = icmp ne [1 x i32]* %1, %end_0
  br i1 %2, label %while_body_0, label %while_end_0
while_body_0:
  %3 = load [1 x i32]*, [1 x i32]** %0
  %4 = call ccc i8 @eclair_btree_value_compare_values_2([1 x i32]* %3, [1 x i32]* %val_0)
  %5 = icmp ne i8 %4, -1
  br i1 %5, label %if_0, label %end_if_0
if_0:
  ret [1 x i32]* %3
end_if_0:
  %6 = getelementptr [1 x i32], [1 x i32]* %3, i32 1
  store [1 x i32]* %6, [1 x i32]** %0
  br label %while_begin_0
while_end_0:
  ret [1 x i32]* %end_0
}

define external ccc [1 x i32]* @eclair_btree_linear_search_upper_bound_2([1 x i32]* %val_0, [1 x i32]* %current_0, [1 x i32]* %end_0) {
start:
  %0 = alloca [1 x i32]*
  store [1 x i32]* %current_0, [1 x i32]** %0
  br label %while_begin_0
while_begin_0:
  %1 = load [1 x i32]*, [1 x i32]** %0
  %2 = icmp ne [1 x i32]* %1, %end_0
  br i1 %2, label %while_body_0, label %while_end_0
while_body_0:
  %3 = load [1 x i32]*, [1 x i32]** %0
  %4 = call ccc i8 @eclair_btree_value_compare_values_2([1 x i32]* %3, [1 x i32]* %val_0)
  %5 = icmp eq i8 %4, 1
  br i1 %5, label %if_0, label %end_if_0
if_0:
  ret [1 x i32]* %3
end_if_0:
  %6 = getelementptr [1 x i32], [1 x i32]* %3, i32 1
  store [1 x i32]* %6, [1 x i32]** %0
  br label %while_begin_0
while_end_0:
  ret [1 x i32]* %end_0
}

define external ccc void @eclair_btree_init_empty_2(%btree_t_2* %tree_0) {
start:
  %0 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 0
  store %node_t_2* zeroinitializer, %node_t_2** %0
  %1 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 1
  store %node_t_2* zeroinitializer, %node_t_2** %1
  ret void
}

define external ccc void @eclair_btree_init_2(%btree_t_2* %tree_0, %btree_iterator_t_2* %start_0, %btree_iterator_t_2* %end_0) {
start:
  call ccc void @eclair_btree_insert_range__2(%btree_t_2* %tree_0, %btree_iterator_t_2* %start_0, %btree_iterator_t_2* %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_2(%btree_t_2* %tree_0) {
start:
  call ccc void @eclair_btree_clear_2(%btree_t_2* %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_2(%btree_t_2* %tree_0) {
start:
  %0 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 0
  %1 = load %node_t_2*, %node_t_2** %0
  %2 = icmp eq %node_t_2* %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_2(%btree_t_2* %tree_0) {
start:
  %0 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 0
  %1 = load %node_t_2*, %node_t_2** %0
  %2 = icmp eq %node_t_2* %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_2(%node_t_2* %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_2() {
start:
  %0 = mul i16 3, 60
  %1 = udiv i16 %0, 4
  %2 = sub i16 60, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_2(%node_t_2* %node_0, %node_t_2** %root_0) {
start:
  %0 = call ccc i16 @eclair_btree_node_split_point_2()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 3
  %3 = load i1, i1* %2
  %4 = call ccc %node_t_2* @eclair_btree_node_new_2(i1 %3)
  %5 = alloca i16
  store i16 0, i16* %5
  br label %for_begin_0
for_begin_0:
  %6 = phi i16 [%1, %start], [%13, %for_body_0]
  %7 = icmp ult i16 %6, 60
  br i1 %7, label %for_body_0, label %for_end_0
for_body_0:
  %8 = load i16, i16* %5
  %9 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 1, i16 %6
  %10 = load [1 x i32], [1 x i32]* %9
  %11 = getelementptr %node_t_2, %node_t_2* %4, i32 0, i32 1, i16 %8
  store [1 x i32] %10, [1 x i32]* %11
  %12 = add i16 1, %8
  store i16 %12, i16* %5
  %13 = add i16 1, %6
  br label %for_begin_0
for_end_0:
  %14 = icmp eq i1 %3, 1
  br i1 %14, label %if_0, label %end_if_0
if_0:
  %15 = bitcast %node_t_2* %4 to %inner_node_t_2*
  %16 = bitcast %node_t_2* %node_0 to %inner_node_t_2*
  store i16 0, i16* %5
  br label %for_begin_1
for_begin_1:
  %17 = phi i16 [%1, %if_0], [%26, %for_body_1]
  %18 = icmp ule i16 %17, 60
  br i1 %18, label %for_body_1, label %for_end_1
for_body_1:
  %19 = load i16, i16* %5
  %20 = getelementptr %inner_node_t_2, %inner_node_t_2* %16, i32 0, i32 1, i16 %17
  %21 = load %node_t_2*, %node_t_2** %20
  %22 = getelementptr %node_t_2, %node_t_2* %21, i32 0, i32 0, i32 0
  store %node_t_2* %4, %node_t_2** %22
  %23 = getelementptr %node_t_2, %node_t_2* %21, i32 0, i32 0, i32 1
  store i16 %19, i16* %23
  %24 = getelementptr %inner_node_t_2, %inner_node_t_2* %15, i32 0, i32 1, i16 %19
  store %node_t_2* %21, %node_t_2** %24
  %25 = add i16 1, %19
  store i16 %25, i16* %5
  %26 = add i16 1, %17
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %27 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 2
  store i16 %0, i16* %27
  %28 = sub i16 60, %0
  %29 = sub i16 %28, 1
  %30 = getelementptr %node_t_2, %node_t_2* %4, i32 0, i32 0, i32 2
  store i16 %29, i16* %30
  call ccc void @eclair_btree_node_grow_parent_2(%node_t_2* %node_0, %node_t_2** %root_0, %node_t_2* %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_2(%node_t_2* %node_0, %node_t_2** %root_0, %node_t_2* %sibling_0) {
start:
  %0 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 0
  %1 = load %node_t_2*, %node_t_2** %0
  %2 = icmp eq %node_t_2* %1, zeroinitializer
  %3 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 2
  %4 = load i16, i16* %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc %node_t_2* @eclair_btree_node_new_2(i1 1)
  %6 = bitcast %node_t_2* %5 to %inner_node_t_2*
  %7 = getelementptr %node_t_2, %node_t_2* %5, i32 0, i32 0, i32 2
  store i16 1, i16* %7
  %8 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 1, i16 %4
  %9 = load [1 x i32], [1 x i32]* %8
  %10 = getelementptr %node_t_2, %node_t_2* %5, i32 0, i32 1, i16 0
  store [1 x i32] %9, [1 x i32]* %10
  %11 = getelementptr %inner_node_t_2, %inner_node_t_2* %6, i32 0, i32 1, i16 0
  store %node_t_2* %node_0, %node_t_2** %11
  %12 = getelementptr %inner_node_t_2, %inner_node_t_2* %6, i32 0, i32 1, i16 1
  store %node_t_2* %sibling_0, %node_t_2** %12
  %13 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 0
  store %node_t_2* %5, %node_t_2** %13
  %14 = getelementptr %node_t_2, %node_t_2* %sibling_0, i32 0, i32 0, i32 0
  store %node_t_2* %5, %node_t_2** %14
  %15 = getelementptr %node_t_2, %node_t_2* %sibling_0, i32 0, i32 0, i32 1
  store i16 1, i16* %15
  store %node_t_2* %5, %node_t_2** %root_0
  ret void
insert_new_node_in_parent_0:
  %16 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 1
  %17 = load i16, i16* %16
  %18 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_2(%node_t_2* %1, %node_t_2** %root_0, i16 %17, %node_t_2* %node_0, [1 x i32]* %18, %node_t_2* %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_2(%node_t_2* %node_0, %node_t_2** %root_0, i16 %pos_0, %node_t_2* %predecessor_0, [1 x i32]* %key_0, %node_t_2* %new_node_0) {
start:
  %0 = alloca i16
  store i16 %pos_0, i16* %0
  %1 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 2
  %2 = load i16, i16* %1
  %3 = icmp uge i16 %2, 60
  br i1 %3, label %if_0, label %end_if_1
if_0:
  %4 = load i16, i16* %0
  %5 = call ccc i16 @eclair_btree_node_rebalance_or_split_2(%node_t_2* %node_0, %node_t_2** %root_0, i16 %pos_0)
  %6 = sub i16 %4, %5
  store i16 %6, i16* %0
  %7 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 2
  %8 = load i16, i16* %7
  %9 = icmp ugt i16 %6, %8
  br i1 %9, label %if_1, label %end_if_0
if_1:
  %10 = sub i16 %6, %8
  %11 = sub i16 %10, 1
  store i16 %11, i16* %0
  %12 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 0
  %13 = load %node_t_2*, %node_t_2** %12
  %14 = bitcast %node_t_2* %13 to %inner_node_t_2*
  %15 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 1
  %16 = load i16, i16* %15
  %17 = add i16 1, %16
  %18 = getelementptr %inner_node_t_2, %inner_node_t_2* %14, i32 0, i32 1, i16 %17
  %19 = load %node_t_2*, %node_t_2** %18
  call ccc void @eclair_btree_node_insert_inner_2(%node_t_2* %19, %node_t_2** %root_0, i16 %11, %node_t_2* %predecessor_0, [1 x i32]* %key_0, %node_t_2* %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %20 = bitcast %node_t_2* %node_0 to %inner_node_t_2*
  %21 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 2
  %22 = load i16, i16* %21
  %23 = sub i16 %22, 1
  %24 = load i16, i16* %0
  br label %for_begin_0
for_begin_0:
  %25 = phi i16 [%23, %end_if_1], [%40, %for_body_0]
  %26 = icmp uge i16 %25, %24
  br i1 %26, label %for_body_0, label %for_end_0
for_body_0:
  %27 = add i16 %25, 1
  %28 = add i16 %25, 2
  %29 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 1, i16 %25
  %30 = load [1 x i32], [1 x i32]* %29
  %31 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 1, i16 %27
  store [1 x i32] %30, [1 x i32]* %31
  %32 = getelementptr %inner_node_t_2, %inner_node_t_2* %20, i32 0, i32 1, i16 %27
  %33 = load %node_t_2*, %node_t_2** %32
  %34 = getelementptr %inner_node_t_2, %inner_node_t_2* %20, i32 0, i32 1, i16 %28
  store %node_t_2* %33, %node_t_2** %34
  %35 = getelementptr %inner_node_t_2, %inner_node_t_2* %20, i32 0, i32 1, i16 %28
  %36 = load %node_t_2*, %node_t_2** %35
  %37 = getelementptr %node_t_2, %node_t_2* %36, i32 0, i32 0, i32 1
  %38 = load i16, i16* %37
  %39 = add i16 1, %38
  store i16 %39, i16* %37
  %40 = sub i16 %25, 1
  br label %for_begin_0
for_end_0:
  %41 = load [1 x i32], [1 x i32]* %key_0
  %42 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 1, i16 %24
  store [1 x i32] %41, [1 x i32]* %42
  %43 = add i16 %24, 1
  %44 = getelementptr %inner_node_t_2, %inner_node_t_2* %20, i32 0, i32 1, i16 %43
  store %node_t_2* %new_node_0, %node_t_2** %44
  %45 = getelementptr %node_t_2, %node_t_2* %new_node_0, i32 0, i32 0, i32 0
  store %node_t_2* %node_0, %node_t_2** %45
  %46 = getelementptr %node_t_2, %node_t_2* %new_node_0, i32 0, i32 0, i32 1
  store i16 %43, i16* %46
  %47 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 2
  %48 = load i16, i16* %47
  %49 = add i16 1, %48
  store i16 %49, i16* %47
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_2(%node_t_2* %node_0, %node_t_2** %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 0
  %1 = load %node_t_2*, %node_t_2** %0
  %2 = bitcast %node_t_2* %1 to %inner_node_t_2*
  %3 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 1
  %4 = load i16, i16* %3
  %5 = icmp ne %inner_node_t_2* %2, zeroinitializer
  %6 = icmp ugt i16 %4, 0
  %7 = and i1 %5, %6
  br i1 %7, label %rebalance_0, label %split_0
rebalance_0:
  %8 = sub i16 %4, 1
  %9 = getelementptr %inner_node_t_2, %inner_node_t_2* %2, i32 0, i32 1, i16 %8
  %10 = load %node_t_2*, %node_t_2** %9
  %11 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 0, i32 2
  %12 = load i16, i16* %11
  %13 = sub i16 60, %12
  %14 = icmp slt i16 %13, %idx_0
  %15 = select i1 %14, i16 %13, i16 %idx_0
  %16 = icmp ugt i16 %15, 0
  br i1 %16, label %if_0, label %end_if_1
if_0:
  %17 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 1
  %18 = load i16, i16* %17
  %19 = sub i16 %18, 1
  %20 = getelementptr %inner_node_t_2, %inner_node_t_2* %2, i32 0, i32 0, i32 1, i16 %19
  %21 = load [1 x i32], [1 x i32]* %20
  %22 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 0, i32 2
  %23 = load i16, i16* %22
  %24 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 1, i16 %23
  store [1 x i32] %21, [1 x i32]* %24
  %25 = sub i16 %15, 1
  br label %for_begin_0
for_begin_0:
  %26 = phi i16 [0, %if_0], [%33, %for_body_0]
  %27 = icmp ult i16 %26, %25
  br i1 %27, label %for_body_0, label %for_end_0
for_body_0:
  %28 = add i16 %23, 1
  %29 = add i16 %26, %28
  %30 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 1, i16 %26
  %31 = load [1 x i32], [1 x i32]* %30
  %32 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 1, i16 %29
  store [1 x i32] %31, [1 x i32]* %32
  %33 = add i16 1, %26
  br label %for_begin_0
for_end_0:
  %34 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 1, i16 %25
  %35 = load [1 x i32], [1 x i32]* %34
  store [1 x i32] %35, [1 x i32]* %20
  %36 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 2
  %37 = load i16, i16* %36
  %38 = sub i16 %37, %15
  br label %for_begin_1
for_begin_1:
  %39 = phi i16 [0, %for_end_0], [%45, %for_body_1]
  %40 = icmp ult i16 %39, %38
  br i1 %40, label %for_body_1, label %for_end_1
for_body_1:
  %41 = add i16 %39, %15
  %42 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 1, i16 %41
  %43 = load [1 x i32], [1 x i32]* %42
  %44 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 1, i16 %39
  store [1 x i32] %43, [1 x i32]* %44
  %45 = add i16 1, %39
  br label %for_begin_1
for_end_1:
  %46 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 3
  %47 = load i1, i1* %46
  %48 = icmp eq i1 %47, 1
  br i1 %48, label %if_1, label %end_if_0
if_1:
  %49 = bitcast %node_t_2* %node_0 to %inner_node_t_2*
  %50 = bitcast %node_t_2* %10 to %inner_node_t_2*
  br label %for_begin_2
for_begin_2:
  %51 = phi i16 [0, %if_1], [%64, %for_body_2]
  %52 = icmp ult i16 %51, %15
  br i1 %52, label %for_body_2, label %for_end_2
for_body_2:
  %53 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 0, i32 2
  %54 = load i16, i16* %53
  %55 = add i16 %54, 1
  %56 = add i16 %51, %55
  %57 = getelementptr %inner_node_t_2, %inner_node_t_2* %49, i32 0, i32 1, i16 %51
  %58 = load %node_t_2*, %node_t_2** %57
  %59 = getelementptr %inner_node_t_2, %inner_node_t_2* %50, i32 0, i32 1, i16 %56
  store %node_t_2* %58, %node_t_2** %59
  %60 = getelementptr %inner_node_t_2, %inner_node_t_2* %50, i32 0, i32 1, i16 %56
  %61 = load %node_t_2*, %node_t_2** %60
  %62 = getelementptr %node_t_2, %node_t_2* %61, i32 0, i32 0, i32 0
  store %node_t_2* %10, %node_t_2** %62
  %63 = getelementptr %node_t_2, %node_t_2* %61, i32 0, i32 0, i32 1
  store i16 %56, i16* %63
  %64 = add i16 1, %51
  br label %for_begin_2
for_end_2:
  %65 = sub i16 %37, %15
  %66 = add i16 1, %65
  br label %for_begin_3
for_begin_3:
  %67 = phi i16 [0, %for_end_2], [%76, %for_body_3]
  %68 = icmp ult i16 %67, %66
  br i1 %68, label %for_body_3, label %for_end_3
for_body_3:
  %69 = add i16 %67, %15
  %70 = getelementptr %inner_node_t_2, %inner_node_t_2* %49, i32 0, i32 1, i16 %69
  %71 = load %node_t_2*, %node_t_2** %70
  %72 = getelementptr %inner_node_t_2, %inner_node_t_2* %49, i32 0, i32 1, i16 %67
  store %node_t_2* %71, %node_t_2** %72
  %73 = getelementptr %inner_node_t_2, %inner_node_t_2* %49, i32 0, i32 1, i16 %67
  %74 = load %node_t_2*, %node_t_2** %73
  %75 = getelementptr %node_t_2, %node_t_2* %74, i32 0, i32 0, i32 1
  store i16 %67, i16* %75
  %76 = add i16 1, %67
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %77 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 0, i32 2
  %78 = load i16, i16* %77
  %79 = add i16 %78, %15
  store i16 %79, i16* %77
  %80 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 2
  %81 = load i16, i16* %80
  %82 = sub i16 %81, %15
  store i16 %82, i16* %80
  ret i16 %15
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_2(%node_t_2* %node_0, %node_t_2** %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_2(%btree_t_2* %tree_0, [1 x i32]* %val_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_2(%btree_t_2* %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc %node_t_2* @eclair_btree_node_new_2(i1 0)
  %2 = getelementptr %node_t_2, %node_t_2* %1, i32 0, i32 0, i32 2
  store i16 1, i16* %2
  %3 = load [1 x i32], [1 x i32]* %val_0
  %4 = getelementptr %node_t_2, %node_t_2* %1, i32 0, i32 1, i16 0
  store [1 x i32] %3, [1 x i32]* %4
  %5 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 0
  store %node_t_2* %1, %node_t_2** %5
  %6 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 1
  store %node_t_2* %1, %node_t_2** %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 0
  %8 = load %node_t_2*, %node_t_2** %7
  %9 = alloca %node_t_2*
  store %node_t_2* %8, %node_t_2** %9
  br label %loop_0
loop_0:
  %10 = load %node_t_2*, %node_t_2** %9
  %11 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 0, i32 3
  %12 = load i1, i1* %11
  %13 = icmp eq i1 %12, 1
  br i1 %13, label %inner_0, label %leaf_0
inner_0:
  %14 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 0, i32 2
  %15 = load i16, i16* %14
  %16 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 1, i16 0
  %17 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 1, i16 %15
  %18 = call ccc [1 x i32]* @eclair_btree_linear_search_lower_bound_2([1 x i32]* %val_0, [1 x i32]* %16, [1 x i32]* %17)
  %19 = ptrtoint [1 x i32]* %18 to i64
  %20 = ptrtoint [1 x i32]* %16 to i64
  %21 = sub i64 %19, %20
  %22 = trunc i64 %21 to i16
  %23 = udiv i16 %22, 4
  %24 = icmp ne [1 x i32]* %18, %17
  %25 = call ccc i8 @eclair_btree_value_compare_values_2([1 x i32]* %18, [1 x i32]* %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %28 = bitcast %node_t_2* %10 to %inner_node_t_2*
  %29 = getelementptr %inner_node_t_2, %inner_node_t_2* %28, i32 0, i32 1, i16 %23
  %30 = load %node_t_2*, %node_t_2** %29
  store %node_t_2* %30, %node_t_2** %9
  br label %loop_0
leaf_0:
  %31 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 0, i32 2
  %32 = load i16, i16* %31
  %33 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 1, i16 0
  %34 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 1, i16 %32
  %35 = call ccc [1 x i32]* @eclair_btree_linear_search_upper_bound_2([1 x i32]* %val_0, [1 x i32]* %33, [1 x i32]* %34)
  %36 = ptrtoint [1 x i32]* %35 to i64
  %37 = ptrtoint [1 x i32]* %33 to i64
  %38 = sub i64 %36, %37
  %39 = trunc i64 %38 to i16
  %40 = udiv i16 %39, 4
  %41 = alloca i16
  store i16 %40, i16* %41
  %42 = icmp ne [1 x i32]* %35, %33
  %43 = getelementptr [1 x i32], [1 x i32]* %35, i32 -1
  %44 = call ccc i8 @eclair_btree_value_compare_values_2([1 x i32]* %43, [1 x i32]* %val_0)
  %45 = icmp eq i8 0, %44
  %46 = and i1 %42, %45
  br i1 %46, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %47 = icmp uge i16 %32, 60
  br i1 %47, label %split_0, label %no_split_0
split_0:
  %48 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 0
  %49 = load i16, i16* %41
  %50 = call ccc i16 @eclair_btree_node_rebalance_or_split_2(%node_t_2* %10, %node_t_2** %48, i16 %49)
  %51 = sub i16 %49, %50
  store i16 %51, i16* %41
  %52 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 0, i32 2
  %53 = load i16, i16* %52
  %54 = icmp ugt i16 %51, %53
  br i1 %54, label %if_0, label %end_if_0
if_0:
  %55 = add i16 %53, 1
  %56 = sub i16 %51, %55
  store i16 %56, i16* %41
  %57 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 0, i32 0
  %58 = load %node_t_2*, %node_t_2** %57
  %59 = bitcast %node_t_2* %58 to %inner_node_t_2*
  %60 = getelementptr %node_t_2, %node_t_2* %10, i32 0, i32 0, i32 1
  %61 = load i16, i16* %60
  %62 = add i16 1, %61
  %63 = getelementptr %inner_node_t_2, %inner_node_t_2* %59, i32 0, i32 1, i16 %62
  %64 = load %node_t_2*, %node_t_2** %63
  store %node_t_2* %64, %node_t_2** %9
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %65 = load %node_t_2*, %node_t_2** %9
  %66 = load i16, i16* %41
  %67 = getelementptr %node_t_2, %node_t_2* %65, i32 0, i32 0, i32 2
  %68 = load i16, i16* %67
  br label %for_begin_0
for_begin_0:
  %69 = phi i16 [%68, %no_split_0], [%75, %for_body_0]
  %70 = icmp ugt i16 %69, %66
  br i1 %70, label %for_body_0, label %for_end_0
for_body_0:
  %71 = sub i16 %69, 1
  %72 = getelementptr %node_t_2, %node_t_2* %65, i32 0, i32 1, i16 %71
  %73 = load [1 x i32], [1 x i32]* %72
  %74 = getelementptr %node_t_2, %node_t_2* %65, i32 0, i32 1, i16 %69
  store [1 x i32] %73, [1 x i32]* %74
  %75 = sub i16 %69, 1
  br label %for_begin_0
for_end_0:
  %76 = load [1 x i32], [1 x i32]* %val_0
  %77 = getelementptr %node_t_2, %node_t_2* %65, i32 0, i32 1, i16 %66
  store [1 x i32] %76, [1 x i32]* %77
  %78 = getelementptr %node_t_2, %node_t_2* %65, i32 0, i32 0, i32 2
  %79 = load i16, i16* %78
  %80 = add i16 1, %79
  store i16 %80, i16* %78
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__2(%btree_t_2* %tree_0, %btree_iterator_t_2* %begin_0, %btree_iterator_t_2* %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_2(%btree_iterator_t_2* %begin_0, %btree_iterator_t_2* %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc [1 x i32]* @eclair_btree_iterator_current_2(%btree_iterator_t_2* %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_2(%btree_t_2* %tree_0, [1 x i32]* %2)
  call ccc void @eclair_btree_iterator_next_2(%btree_iterator_t_2* %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_2(%btree_t_2* %tree_0, %btree_iterator_t_2* %result_0) {
start:
  %0 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 1
  %1 = load %node_t_2*, %node_t_2** %0
  %2 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %result_0, i32 0, i32 0
  store %node_t_2* %1, %node_t_2** %2
  %3 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %result_0, i32 0, i32 1
  store i16 0, i16* %3
  ret void
}

define external ccc void @eclair_btree_end_2(%btree_t_2* %tree_0, %btree_iterator_t_2* %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_2(%btree_iterator_t_2* %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_2(%btree_t_2* %tree_0, [1 x i32]* %val_0) {
start:
  %0 = alloca %btree_iterator_t_2, i32 1
  %1 = alloca %btree_iterator_t_2, i32 1
  call ccc void @eclair_btree_find_2(%btree_t_2* %tree_0, [1 x i32]* %val_0, %btree_iterator_t_2* %0)
  call ccc void @eclair_btree_end_2(%btree_t_2* %tree_0, %btree_iterator_t_2* %1)
  %2 = call ccc i1 @eclair_btree_iterator_is_equal_2(%btree_iterator_t_2* %0, %btree_iterator_t_2* %1)
  %3 = select i1 %2, i1 0, i1 1
  ret i1 %3
}

define external ccc void @eclair_btree_find_2(%btree_t_2* %tree_0, [1 x i32]* %val_0, %btree_iterator_t_2* %result_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_2(%btree_t_2* %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_2(%btree_iterator_t_2* %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 0
  %2 = load %node_t_2*, %node_t_2** %1
  %3 = alloca %node_t_2*
  store %node_t_2* %2, %node_t_2** %3
  br label %loop_0
loop_0:
  %4 = load %node_t_2*, %node_t_2** %3
  %5 = getelementptr %node_t_2, %node_t_2* %4, i32 0, i32 0, i32 2
  %6 = load i16, i16* %5
  %7 = getelementptr %node_t_2, %node_t_2* %4, i32 0, i32 1, i16 0
  %8 = getelementptr %node_t_2, %node_t_2* %4, i32 0, i32 1, i16 %6
  %9 = call ccc [1 x i32]* @eclair_btree_linear_search_lower_bound_2([1 x i32]* %val_0, [1 x i32]* %7, [1 x i32]* %8)
  %10 = ptrtoint [1 x i32]* %9 to i64
  %11 = ptrtoint [1 x i32]* %7 to i64
  %12 = sub i64 %10, %11
  %13 = trunc i64 %12 to i16
  %14 = udiv i16 %13, 4
  %15 = icmp ult [1 x i32]* %9, %8
  %16 = call ccc i8 @eclair_btree_value_compare_values_2([1 x i32]* %9, [1 x i32]* %val_0)
  %17 = icmp eq i8 0, %16
  %18 = and i1 %15, %17
  br i1 %18, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_2(%btree_iterator_t_2* %result_0, %node_t_2* %4, i16 %14)
  ret void
end_if_1:
  %19 = getelementptr %node_t_2, %node_t_2* %4, i32 0, i32 0, i32 3
  %20 = load i1, i1* %19
  %21 = icmp eq i1 %20, 0
  br i1 %21, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_2(%btree_iterator_t_2* %result_0)
  ret void
end_if_2:
  %22 = bitcast %node_t_2* %4 to %inner_node_t_2*
  %23 = getelementptr %inner_node_t_2, %inner_node_t_2* %22, i32 0, i32 1, i16 %14
  %24 = load %node_t_2*, %node_t_2** %23
  store %node_t_2* %24, %node_t_2** %3
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_2(%btree_t_2* %tree_0, [1 x i32]* %val_0, %btree_iterator_t_2* %result_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_2(%btree_t_2* %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_2(%btree_iterator_t_2* %result_0)
  ret void
end_if_0:
  %1 = alloca %btree_iterator_t_2, i32 1
  call ccc void @eclair_btree_iterator_end_init_2(%btree_iterator_t_2* %1)
  %2 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 0
  %3 = load %node_t_2*, %node_t_2** %2
  %4 = alloca %node_t_2*
  store %node_t_2* %3, %node_t_2** %4
  br label %loop_0
loop_0:
  %5 = load %node_t_2*, %node_t_2** %4
  %6 = getelementptr %node_t_2, %node_t_2* %5, i32 0, i32 0, i32 2
  %7 = load i16, i16* %6
  %8 = getelementptr %node_t_2, %node_t_2* %5, i32 0, i32 1, i16 0
  %9 = getelementptr %node_t_2, %node_t_2* %5, i32 0, i32 1, i16 %7
  %10 = call ccc [1 x i32]* @eclair_btree_linear_search_lower_bound_2([1 x i32]* %val_0, [1 x i32]* %8, [1 x i32]* %9)
  %11 = ptrtoint [1 x i32]* %10 to i64
  %12 = ptrtoint [1 x i32]* %8 to i64
  %13 = sub i64 %11, %12
  %14 = trunc i64 %13 to i16
  %15 = udiv i16 %14, 4
  %16 = getelementptr %node_t_2, %node_t_2* %5, i32 0, i32 0, i32 3
  %17 = load i1, i1* %16
  %18 = icmp eq i1 %17, 0
  br i1 %18, label %if_1, label %end_if_1
if_1:
  %19 = icmp eq [1 x i32]* %10, %9
  br i1 %19, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %20 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %1, i32 0, i32 0
  %21 = load %node_t_2*, %node_t_2** %20
  %22 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %result_0, i32 0, i32 0
  store %node_t_2* %21, %node_t_2** %22
  %23 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %1, i32 0, i32 1
  %24 = load i16, i16* %23
  %25 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %result_0, i32 0, i32 1
  store i16 %24, i16* %25
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_2(%btree_iterator_t_2* %result_0, %node_t_2* %5, i16 %15)
  ret void
end_if_1:
  %26 = icmp ne [1 x i32]* %10, %9
  %27 = call ccc i8 @eclair_btree_value_compare_values_2([1 x i32]* %10, [1 x i32]* %val_0)
  %28 = icmp eq i8 0, %27
  %29 = and i1 %26, %28
  br i1 %29, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_2(%btree_iterator_t_2* %result_0, %node_t_2* %5, i16 %15)
  ret void
end_if_2:
  br i1 %26, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_2(%btree_iterator_t_2* %1, %node_t_2* %5, i16 %15)
  br label %end_if_3
end_if_3:
  %30 = bitcast %node_t_2* %5 to %inner_node_t_2*
  %31 = getelementptr %inner_node_t_2, %inner_node_t_2* %30, i32 0, i32 1, i16 %15
  %32 = load %node_t_2*, %node_t_2** %31
  store %node_t_2* %32, %node_t_2** %4
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_2(%btree_t_2* %tree_0, [1 x i32]* %val_0, %btree_iterator_t_2* %result_0) {
start:
  %0 = call ccc i1 @eclair_btree_is_empty_2(%btree_t_2* %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_2(%btree_iterator_t_2* %result_0)
  ret void
end_if_0:
  %1 = alloca %btree_iterator_t_2, i32 1
  call ccc void @eclair_btree_iterator_end_init_2(%btree_iterator_t_2* %1)
  %2 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 0
  %3 = load %node_t_2*, %node_t_2** %2
  %4 = alloca %node_t_2*
  store %node_t_2* %3, %node_t_2** %4
  br label %loop_0
loop_0:
  %5 = load %node_t_2*, %node_t_2** %4
  %6 = getelementptr %node_t_2, %node_t_2* %5, i32 0, i32 0, i32 2
  %7 = load i16, i16* %6
  %8 = getelementptr %node_t_2, %node_t_2* %5, i32 0, i32 1, i16 0
  %9 = getelementptr %node_t_2, %node_t_2* %5, i32 0, i32 1, i16 %7
  %10 = call ccc [1 x i32]* @eclair_btree_linear_search_upper_bound_2([1 x i32]* %val_0, [1 x i32]* %8, [1 x i32]* %9)
  %11 = ptrtoint [1 x i32]* %10 to i64
  %12 = ptrtoint [1 x i32]* %8 to i64
  %13 = sub i64 %11, %12
  %14 = trunc i64 %13 to i16
  %15 = udiv i16 %14, 4
  %16 = getelementptr %node_t_2, %node_t_2* %5, i32 0, i32 0, i32 3
  %17 = load i1, i1* %16
  %18 = icmp eq i1 %17, 0
  br i1 %18, label %if_1, label %end_if_1
if_1:
  %19 = icmp eq [1 x i32]* %10, %9
  br i1 %19, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %20 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %1, i32 0, i32 0
  %21 = load %node_t_2*, %node_t_2** %20
  %22 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %result_0, i32 0, i32 0
  store %node_t_2* %21, %node_t_2** %22
  %23 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %1, i32 0, i32 1
  %24 = load i16, i16* %23
  %25 = getelementptr %btree_iterator_t_2, %btree_iterator_t_2* %result_0, i32 0, i32 1
  store i16 %24, i16* %25
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_2(%btree_iterator_t_2* %result_0, %node_t_2* %5, i16 %15)
  ret void
end_if_1:
  %26 = icmp ne [1 x i32]* %10, %9
  br i1 %26, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_2(%btree_iterator_t_2* %result_0, %node_t_2* %5, i16 %15)
  br label %end_if_2
end_if_2:
  %27 = bitcast %node_t_2* %5 to %inner_node_t_2*
  %28 = getelementptr %inner_node_t_2, %inner_node_t_2* %27, i32 0, i32 1, i16 %15
  %29 = load %node_t_2*, %node_t_2** %28
  store %node_t_2* %29, %node_t_2** %4
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_2(%node_t_2* %node_0) {
start:
  %0 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 3
  %1 = load i1, i1* %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = bitcast %node_t_2* %node_0 to %inner_node_t_2*
  %4 = getelementptr %node_t_2, %node_t_2* %node_0, i32 0, i32 0, i32 2
  %5 = load i16, i16* %4
  br label %for_begin_0
for_begin_0:
  %6 = phi i16 [0, %if_0], [%11, %end_if_0]
  %7 = icmp ule i16 %6, %5
  br i1 %7, label %for_body_0, label %for_end_0
for_body_0:
  %8 = getelementptr %inner_node_t_2, %inner_node_t_2* %3, i32 0, i32 1, i16 %6
  %9 = load %node_t_2*, %node_t_2** %8
  %10 = icmp ne %node_t_2* %9, zeroinitializer
  br i1 %10, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_2(%node_t_2* %9)
  br label %end_if_0
end_if_0:
  %11 = add i16 1, %6
  br label %for_begin_0
for_end_0:
  br label %end_if_1
end_if_1:
  %12 = bitcast %node_t_2* %node_0 to i8*
  call ccc void @free(i8* %12)
  ret void
}

define external ccc void @eclair_btree_clear_2(%btree_t_2* %tree_0) {
start:
  %0 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 0
  %1 = load %node_t_2*, %node_t_2** %0
  %2 = icmp ne %node_t_2* %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_2(%node_t_2* %1)
  %3 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 0
  store %node_t_2* zeroinitializer, %node_t_2** %3
  %4 = getelementptr %btree_t_2, %btree_t_2* %tree_0, i32 0, i32 1
  store %node_t_2* zeroinitializer, %node_t_2** %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_2(%btree_t_2* %lhs_0, %btree_t_2* %rhs_0) {
start:
  %0 = getelementptr %btree_t_2, %btree_t_2* %lhs_0, i32 0, i32 0
  %1 = load %node_t_2*, %node_t_2** %0
  %2 = getelementptr %btree_t_2, %btree_t_2* %rhs_0, i32 0, i32 0
  %3 = load %node_t_2*, %node_t_2** %2
  %4 = getelementptr %btree_t_2, %btree_t_2* %lhs_0, i32 0, i32 0
  store %node_t_2* %3, %node_t_2** %4
  %5 = getelementptr %btree_t_2, %btree_t_2* %rhs_0, i32 0, i32 0
  store %node_t_2* %1, %node_t_2** %5
  %6 = getelementptr %btree_t_2, %btree_t_2* %lhs_0, i32 0, i32 1
  %7 = load %node_t_2*, %node_t_2** %6
  %8 = getelementptr %btree_t_2, %btree_t_2* %rhs_0, i32 0, i32 1
  %9 = load %node_t_2*, %node_t_2** %8
  %10 = getelementptr %btree_t_2, %btree_t_2* %lhs_0, i32 0, i32 1
  store %node_t_2* %9, %node_t_2** %10
  %11 = getelementptr %btree_t_2, %btree_t_2* %rhs_0, i32 0, i32 1
  store %node_t_2* %7, %node_t_2** %11
  ret void
}

@specialize_debug_info.btree__1__0__256__linear = global i32 2

@specialize_debug_info.btree__2__0_1__256__linear = global i32 1

@specialize_debug_info.btree__3__0_1_2__256__linear = global i32 0

%symbol_t = type <{i32, i8*}>

define external ccc void @eclair_symbol_init(%symbol_t* %symbol_0, i32 %size_0, i8* %data_0) {
start:
  %0 = getelementptr %symbol_t, %symbol_t* %symbol_0, i32 0, i32 0
  store i32 %size_0, i32* %0
  %1 = getelementptr %symbol_t, %symbol_t* %symbol_0, i32 0, i32 1
  store i8* %data_0, i8** %1
  ret void
}

define external ccc void @eclair_symbol_destroy(%symbol_t* %symbol_0) {
start:
  %0 = getelementptr %symbol_t, %symbol_t* %symbol_0, i32 0, i32 1
  %1 = load i8*, i8** %0
  call ccc void @free(i8* %1)
  ret void
}

define external ccc i1 @eclair_symbol_is_equal(%symbol_t* %symbol1_0, %symbol_t* %symbol2_0) {
start:
  %0 = getelementptr %symbol_t, %symbol_t* %symbol1_0, i32 0, i32 0
  %1 = load i32, i32* %0
  %2 = getelementptr %symbol_t, %symbol_t* %symbol2_0, i32 0, i32 0
  %3 = load i32, i32* %2
  %4 = icmp ne i32 %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %symbol_t, %symbol_t* %symbol1_0, i32 0, i32 1
  %6 = load i8*, i8** %5
  %7 = getelementptr %symbol_t, %symbol_t* %symbol2_0, i32 0, i32 1
  %8 = load i8*, i8** %7
  %9 = zext i32 %1 to i64
  %10 = call ccc i32 @memcmp(i8* %6, i8* %8, i64 %9)
  %11 = icmp eq i32 %10, 0
  ret i1 %11
}

%vector_t_symbol = type {%symbol_t*, %symbol_t*, i32}

define external ccc void @eclair_vector_init_symbol(%vector_t_symbol* %vec_0) {
start:
  %0 = call ccc i8* @malloc(i32 192)
  %1 = bitcast i8* %0 to %symbol_t*
  %2 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 0
  store %symbol_t* %1, %symbol_t** %2
  %3 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 1
  store %symbol_t* %1, %symbol_t** %3
  %4 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 2
  store i32 16, i32* %4
  ret void
}

define external ccc void @eclair_vector_destroy_symbol(%vector_t_symbol* %vec_0) {
start:
  %0 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 0
  %1 = load %symbol_t*, %symbol_t** %0
  %2 = alloca %symbol_t*
  store %symbol_t* %1, %symbol_t** %2
  br label %while_begin_0
while_begin_0:
  %3 = load %symbol_t*, %symbol_t** %2
  %4 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 1
  %5 = load %symbol_t*, %symbol_t** %4
  %6 = icmp ne %symbol_t* %3, %5
  br i1 %6, label %while_body_0, label %while_end_0
while_body_0:
  %7 = load %symbol_t*, %symbol_t** %2
  call ccc void @eclair_symbol_destroy(%symbol_t* %7)
  %8 = getelementptr %symbol_t, %symbol_t* %7, i32 1
  store %symbol_t* %8, %symbol_t** %2
  br label %while_begin_0
while_end_0:
  %9 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 0
  %10 = load %symbol_t*, %symbol_t** %9
  %11 = bitcast %symbol_t* %10 to i8*
  call ccc void @free(i8* %11)
  ret void
}

define external ccc i32 @eclair_vector_size_symbol(%vector_t_symbol* %vec_0) {
start:
  %0 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 0
  %1 = load %symbol_t*, %symbol_t** %0
  %2 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 1
  %3 = load %symbol_t*, %symbol_t** %2
  %4 = ptrtoint %symbol_t* %3 to i64
  %5 = ptrtoint %symbol_t* %1 to i64
  %6 = sub i64 %4, %5
  %7 = trunc i64 %6 to i32
  %8 = udiv i32 %7, 12
  ret i32 %8
}

define external ccc void @eclair_vector_grow_symbol(%vector_t_symbol* %vec_0) {
start:
  %0 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 2
  %1 = load i32, i32* %0
  %2 = mul i32 %1, 12
  %3 = zext i32 %2 to i64
  %4 = mul i32 %1, 2
  %5 = mul i32 %4, 12
  %6 = call ccc i8* @malloc(i32 %5)
  %7 = bitcast i8* %6 to %symbol_t*
  %8 = getelementptr %symbol_t, %symbol_t* %7, i32 %1
  %9 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 0
  %10 = load %symbol_t*, %symbol_t** %9
  %11 = bitcast %symbol_t* %10 to i8*
  %12 = bitcast %symbol_t* %7 to i8*
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %11, i64 %3, i1 0)
  call ccc void @free(i8* %11)
  %13 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 0
  store %symbol_t* %7, %symbol_t** %13
  %14 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 1
  store %symbol_t* %8, %symbol_t** %14
  %15 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 2
  store i32 %4, i32* %15
  ret void
}

define external ccc i32 @eclair_vector_push_symbol(%vector_t_symbol* %vec_0, %symbol_t* %elem_0) {
start:
  %0 = call ccc i32 @eclair_vector_size_symbol(%vector_t_symbol* %vec_0)
  %1 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 2
  %2 = load i32, i32* %1
  %3 = icmp eq i32 %0, %2
  br i1 %3, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_vector_grow_symbol(%vector_t_symbol* %vec_0)
  br label %end_if_0
end_if_0:
  %4 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 1
  %5 = load %symbol_t*, %symbol_t** %4
  %6 = load %symbol_t, %symbol_t* %elem_0
  store %symbol_t %6, %symbol_t* %5
  %7 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 1
  %8 = load %symbol_t*, %symbol_t** %7
  %9 = getelementptr %symbol_t, %symbol_t* %8, i32 1
  store %symbol_t* %9, %symbol_t** %7
  ret i32 %0
}

define external ccc %symbol_t* @eclair_vector_get_value_symbol(%vector_t_symbol* %vec_0, i32 %idx_0) {
start:
  %0 = getelementptr %vector_t_symbol, %vector_t_symbol* %vec_0, i32 0, i32 0
  %1 = load %symbol_t*, %symbol_t** %0
  %2 = getelementptr %symbol_t, %symbol_t* %1, i32 %idx_0
  ret %symbol_t* %2
}

%entry_t = type {%symbol_t, i32}

%vector_t_entry = type {%entry_t*, %entry_t*, i32}

define external ccc void @eclair_vector_init_entry(%vector_t_entry* %vec_0) {
start:
  %0 = call ccc i8* @malloc(i32 256)
  %1 = bitcast i8* %0 to %entry_t*
  %2 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 0
  store %entry_t* %1, %entry_t** %2
  %3 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 1
  store %entry_t* %1, %entry_t** %3
  %4 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 2
  store i32 16, i32* %4
  ret void
}

define external ccc void @eclair_vector_destroy_entry(%vector_t_entry* %vec_0) {
start:
  %0 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 0
  %1 = load %entry_t*, %entry_t** %0
  %2 = bitcast %entry_t* %1 to i8*
  call ccc void @free(i8* %2)
  ret void
}

define external ccc i32 @eclair_vector_size_entry(%vector_t_entry* %vec_0) {
start:
  %0 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 0
  %1 = load %entry_t*, %entry_t** %0
  %2 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 1
  %3 = load %entry_t*, %entry_t** %2
  %4 = ptrtoint %entry_t* %3 to i64
  %5 = ptrtoint %entry_t* %1 to i64
  %6 = sub i64 %4, %5
  %7 = trunc i64 %6 to i32
  %8 = udiv i32 %7, 16
  ret i32 %8
}

define external ccc void @eclair_vector_grow_entry(%vector_t_entry* %vec_0) {
start:
  %0 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 2
  %1 = load i32, i32* %0
  %2 = mul i32 %1, 16
  %3 = zext i32 %2 to i64
  %4 = mul i32 %1, 2
  %5 = mul i32 %4, 16
  %6 = call ccc i8* @malloc(i32 %5)
  %7 = bitcast i8* %6 to %entry_t*
  %8 = getelementptr %entry_t, %entry_t* %7, i32 %1
  %9 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 0
  %10 = load %entry_t*, %entry_t** %9
  %11 = bitcast %entry_t* %10 to i8*
  %12 = bitcast %entry_t* %7 to i8*
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %11, i64 %3, i1 0)
  call ccc void @free(i8* %11)
  %13 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 0
  store %entry_t* %7, %entry_t** %13
  %14 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 1
  store %entry_t* %8, %entry_t** %14
  %15 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 2
  store i32 %4, i32* %15
  ret void
}

define external ccc i32 @eclair_vector_push_entry(%vector_t_entry* %vec_0, %entry_t* %elem_0) {
start:
  %0 = call ccc i32 @eclair_vector_size_entry(%vector_t_entry* %vec_0)
  %1 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 2
  %2 = load i32, i32* %1
  %3 = icmp eq i32 %0, %2
  br i1 %3, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_vector_grow_entry(%vector_t_entry* %vec_0)
  br label %end_if_0
end_if_0:
  %4 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 1
  %5 = load %entry_t*, %entry_t** %4
  %6 = load %entry_t, %entry_t* %elem_0
  store %entry_t %6, %entry_t* %5
  %7 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 1
  %8 = load %entry_t*, %entry_t** %7
  %9 = getelementptr %entry_t, %entry_t* %8, i32 1
  store %entry_t* %9, %entry_t** %7
  ret i32 %0
}

define external ccc %entry_t* @eclair_vector_get_value_entry(%vector_t_entry* %vec_0, i32 %idx_0) {
start:
  %0 = getelementptr %vector_t_entry, %vector_t_entry* %vec_0, i32 0, i32 0
  %1 = load %entry_t*, %entry_t** %0
  %2 = getelementptr %entry_t, %entry_t* %1, i32 %idx_0
  ret %entry_t* %2
}

%hashmap_t = type {[64 x %vector_t_entry]}

define external ccc i32 @eclair_symbol_hash(%symbol_t* %symbol_0) {
start:
  %0 = alloca i32
  store i32 0, i32* %0
  %1 = getelementptr %symbol_t, %symbol_t* %symbol_0, i32 0, i32 0
  %2 = load i32, i32* %1
  %3 = getelementptr %symbol_t, %symbol_t* %symbol_0, i32 0, i32 1
  %4 = load i8*, i8** %3
  br label %for_begin_0
for_begin_0:
  %5 = phi i32 [0, %start], [%13, %for_body_0]
  %6 = icmp ult i32 %5, %2
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = getelementptr i8, i8* %4, i32 %5
  %8 = load i8, i8* %7
  %9 = zext i8 %8 to i32
  %10 = load i32, i32* %0
  %11 = mul i32 31, %10
  %12 = add i32 %9, %11
  store i32 %12, i32* %0
  %13 = add i32 1, %5
  br label %for_begin_0
for_end_0:
  %14 = load i32, i32* %0
  %15 = and i32 %14, 63
  ret i32 %15
}

define external ccc void @eclair_hashmap_init(%hashmap_t* %hashmap_0) {
start:
  br label %for_begin_0
for_begin_0:
  %0 = phi i32 [0, %start], [%3, %for_body_0]
  %1 = icmp ult i32 %0, 64
  br i1 %1, label %for_body_0, label %for_end_0
for_body_0:
  %2 = getelementptr %hashmap_t, %hashmap_t* %hashmap_0, i32 0, i32 0, i32 %0
  call ccc void @eclair_vector_init_entry(%vector_t_entry* %2)
  %3 = add i32 1, %0
  br label %for_begin_0
for_end_0:
  ret void
}

define external ccc void @eclair_hashmap_destroy(%hashmap_t* %hashmap_0) {
start:
  br label %for_begin_0
for_begin_0:
  %0 = phi i32 [0, %start], [%3, %for_body_0]
  %1 = icmp ult i32 %0, 64
  br i1 %1, label %for_body_0, label %for_end_0
for_body_0:
  %2 = getelementptr %hashmap_t, %hashmap_t* %hashmap_0, i32 0, i32 0, i32 %0
  call ccc void @eclair_vector_destroy_entry(%vector_t_entry* %2)
  %3 = add i32 1, %0
  br label %for_begin_0
for_end_0:
  ret void
}

define external ccc i32 @eclair_hashmap_get_or_put_value(%hashmap_t* %hashmap_0, %symbol_t* %symbol_0, i32 %value_0) {
start:
  %0 = call ccc i32 @eclair_symbol_hash(%symbol_t* %symbol_0)
  %1 = and i32 %0, 63
  %2 = getelementptr %hashmap_t, %hashmap_t* %hashmap_0, i32 0, i32 0, i32 %1
  %3 = call ccc i32 @eclair_vector_size_entry(%vector_t_entry* %2)
  br label %for_begin_0
for_begin_0:
  %4 = phi i32 [0, %start], [%11, %end_if_0]
  %5 = icmp ult i32 %4, %3
  br i1 %5, label %for_body_0, label %for_end_0
for_body_0:
  %6 = call ccc %entry_t* @eclair_vector_get_value_entry(%vector_t_entry* %2, i32 %4)
  %7 = getelementptr %entry_t, %entry_t* %6, i32 0, i32 0
  %8 = call ccc i1 @eclair_symbol_is_equal(%symbol_t* %7, %symbol_t* %symbol_0)
  br i1 %8, label %if_0, label %end_if_0
if_0:
  %9 = getelementptr %entry_t, %entry_t* %6, i32 0, i32 1
  %10 = load i32, i32* %9
  ret i32 %10
end_if_0:
  %11 = add i32 1, %4
  br label %for_begin_0
for_end_0:
  %12 = load %symbol_t, %symbol_t* %symbol_0
  %13 = alloca %entry_t
  %14 = getelementptr %entry_t, %entry_t* %13, i32 0, i32 0
  store %symbol_t %12, %symbol_t* %14
  %15 = getelementptr %entry_t, %entry_t* %13, i32 0, i32 1
  store i32 %value_0, i32* %15
  %16 = call ccc i32 @eclair_vector_push_entry(%vector_t_entry* %2, %entry_t* %13)
  ret i32 %value_0
}

define external ccc i32 @eclair_hashmap_lookup(%hashmap_t* %hashmap_0, %symbol_t* %symbol_0) {
start:
  %0 = call ccc i32 @eclair_symbol_hash(%symbol_t* %symbol_0)
  %1 = and i32 %0, 63
  %2 = getelementptr %hashmap_t, %hashmap_t* %hashmap_0, i32 0, i32 0, i32 %1
  %3 = call ccc i32 @eclair_vector_size_entry(%vector_t_entry* %2)
  br label %for_begin_0
for_begin_0:
  %4 = phi i32 [0, %start], [%11, %end_if_0]
  %5 = icmp ult i32 %4, %3
  br i1 %5, label %for_body_0, label %for_end_0
for_body_0:
  %6 = call ccc %entry_t* @eclair_vector_get_value_entry(%vector_t_entry* %2, i32 %4)
  %7 = getelementptr %entry_t, %entry_t* %6, i32 0, i32 0
  %8 = call ccc i1 @eclair_symbol_is_equal(%symbol_t* %7, %symbol_t* %symbol_0)
  br i1 %8, label %if_0, label %end_if_0
if_0:
  %9 = getelementptr %entry_t, %entry_t* %6, i32 0, i32 1
  %10 = load i32, i32* %9
  ret i32 %10
end_if_0:
  %11 = add i32 1, %4
  br label %for_begin_0
for_end_0:
  ret i32 4294967295
}

define external ccc i1 @eclair_hashmap_contains(%hashmap_t* %hashmap_0, %symbol_t* %symbol_0) {
start:
  %0 = call ccc i32 @eclair_symbol_hash(%symbol_t* %symbol_0)
  %1 = and i32 %0, 63
  %2 = getelementptr %hashmap_t, %hashmap_t* %hashmap_0, i32 0, i32 0, i32 %1
  %3 = call ccc i32 @eclair_vector_size_entry(%vector_t_entry* %2)
  br label %for_begin_0
for_begin_0:
  %4 = phi i32 [0, %start], [%9, %end_if_0]
  %5 = icmp ult i32 %4, %3
  br i1 %5, label %for_body_0, label %for_end_0
for_body_0:
  %6 = call ccc %entry_t* @eclair_vector_get_value_entry(%vector_t_entry* %2, i32 %4)
  %7 = getelementptr %entry_t, %entry_t* %6, i32 0, i32 0
  %8 = call ccc i1 @eclair_symbol_is_equal(%symbol_t* %7, %symbol_t* %symbol_0)
  br i1 %8, label %if_0, label %end_if_0
if_0:
  ret i1 1
end_if_0:
  %9 = add i32 1, %4
  br label %for_begin_0
for_end_0:
  ret i1 0
}

%symbol_table = type {%vector_t_symbol, %hashmap_t}

define external ccc void @eclair_symbol_table_init(%symbol_table* %table_0) {
start:
  %0 = getelementptr %symbol_table, %symbol_table* %table_0, i32 0, i32 0
  %1 = getelementptr %symbol_table, %symbol_table* %table_0, i32 0, i32 1
  call ccc void @eclair_vector_init_symbol(%vector_t_symbol* %0)
  call ccc void @eclair_hashmap_init(%hashmap_t* %1)
  ret void
}

define external ccc void @eclair_symbol_table_destroy(%symbol_table* %table_0) {
start:
  %0 = getelementptr %symbol_table, %symbol_table* %table_0, i32 0, i32 0
  %1 = getelementptr %symbol_table, %symbol_table* %table_0, i32 0, i32 1
  call ccc void @eclair_vector_destroy_symbol(%vector_t_symbol* %0)
  call ccc void @eclair_hashmap_destroy(%hashmap_t* %1)
  ret void
}

define external ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %table_0, %symbol_t* %symbol_0) {
start:
  %0 = getelementptr %symbol_table, %symbol_table* %table_0, i32 0, i32 0
  %1 = getelementptr %symbol_table, %symbol_table* %table_0, i32 0, i32 1
  %2 = call ccc i32 @eclair_vector_size_symbol(%vector_t_symbol* %0)
  %3 = call ccc i32 @eclair_hashmap_get_or_put_value(%hashmap_t* %1, %symbol_t* %symbol_0, i32 %2)
  %4 = icmp eq i32 %2, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  %5 = call ccc i32 @eclair_vector_push_symbol(%vector_t_symbol* %0, %symbol_t* %symbol_0)
  br label %end_if_0
end_if_0:
  ret i32 %3
}

define external ccc i1 @eclair_symbol_table_contains_index(%symbol_table* %table_0, i32 %index_0) {
start:
  %0 = getelementptr %symbol_table, %symbol_table* %table_0, i32 0, i32 0
  %1 = call ccc i32 @eclair_vector_size_symbol(%vector_t_symbol* %0)
  %2 = icmp ult i32 %index_0, %1
  ret i1 %2
}

define external ccc i1 @eclair_symbol_table_contains_symbol(%symbol_table* %table_0, %symbol_t* %symbol_0) {
start:
  %0 = getelementptr %symbol_table, %symbol_table* %table_0, i32 0, i32 1
  %1 = call ccc i1 @eclair_hashmap_contains(%hashmap_t* %0, %symbol_t* %symbol_0)
  ret i1 %1
}

define external ccc i32 @eclair_symbol_table_lookup_index(%symbol_table* %table_0, %symbol_t* %symbol_0) {
start:
  %0 = getelementptr %symbol_table, %symbol_table* %table_0, i32 0, i32 1
  %1 = call ccc i32 @eclair_hashmap_lookup(%hashmap_t* %0, %symbol_t* %symbol_0)
  ret i32 %1
}

define external ccc %symbol_t* @eclair_symbol_table_lookup_symbol(%symbol_table* %table_0, i32 %index_0) {
start:
  %0 = getelementptr %symbol_table, %symbol_table* %table_0, i32 0, i32 0
  %1 = call ccc %symbol_t* @eclair_vector_get_value_symbol(%vector_t_symbol* %0, i32 %index_0)
  ret %symbol_t* %1
}

%program = type {%symbol_table, %btree_t_0, %btree_t_1, %btree_t_2, %btree_t_2, %btree_t_2}

@string_literal_0 = global [22 x i8] [i8 117, i8 115, i8 101, i8 114, i8 95, i8 112, i8 97, i8 99, i8 107, i8 97, i8 103, i8 101, i8 95, i8 99, i8 97, i8 116, i8 101, i8 103, i8 111, i8 114, i8 121, i8 0]

@string_literal_1 = global [28 x i8] [i8 110, i8 111, i8 114, i8 109, i8 97, i8 108, i8 105, i8 115, i8 101, i8 100, i8 95, i8 112, i8 97, i8 99, i8 107, i8 97, i8 103, i8 101, i8 95, i8 99, i8 97, i8 116, i8 101, i8 103, i8 111, i8 114, i8 121, i8 0]

@string_literal_2 = global [16 x i8] [i8 110, i8 111, i8 114, i8 109, i8 97, i8 108, i8 105, i8 115, i8 101, i8 95, i8 105, i8 115, i8 115, i8 117, i8 101, i8 0]

@string_literal_3 = global [15 x i8] [i8 102, i8 108, i8 111, i8 114, i8 97, i8 95, i8 99, i8 97, i8 116, i8 101, i8 103, i8 111, i8 114, i8 121, i8 0]

@string_literal_4 = global [11 x i8] [i8 97, i8 108, i8 103, i8 111, i8 114, i8 105, i8 116, i8 104, i8 109, i8 115, i8 0]

@string_literal_5 = global [11 x i8] [i8 65, i8 108, i8 103, i8 111, i8 114, i8 105, i8 116, i8 104, i8 109, i8 115, i8 0]

@string_literal_6 = global [59 x i8] [i8 65, i8 108, i8 103, i8 111, i8 114, i8 105, i8 116, i8 104, i8 109, i8 115, i8 32, i8 105, i8 109, i8 112, i8 108, i8 101, i8 109, i8 101, i8 110, i8 116, i8 101, i8 100, i8 32, i8 105, i8 110, i8 32, i8 72, i8 97, i8 115, i8 107, i8 101, i8 108, i8 108, i8 44, i8 32, i8 108, i8 105, i8 107, i8 101, i8 32, i8 115, i8 111, i8 114, i8 116, i8 105, i8 110, i8 103, i8 44, i8 32, i8 115, i8 101, i8 97, i8 114, i8 99, i8 104, i8 105, i8 110, i8 103, i8 0]

@string_literal_7 = global [6 x i8] [i8 97, i8 117, i8 100, i8 105, i8 111, i8 0]

@string_literal_8 = global [6 x i8] [i8 65, i8 117, i8 100, i8 105, i8 111, i8 0]

@string_literal_9 = global [35 x i8] [i8 80, i8 114, i8 111, i8 99, i8 101, i8 115, i8 115, i8 32, i8 100, i8 105, i8 103, i8 105, i8 116, i8 97, i8 108, i8 32, i8 115, i8 105, i8 103, i8 110, i8 97, i8 108, i8 44, i8 32, i8 109, i8 97, i8 107, i8 101, i8 32, i8 109, i8 117, i8 115, i8 105, i8 99, i8 0]

@string_literal_10 = global [15 x i8] [i8 98, i8 105, i8 111, i8 105, i8 110, i8 102, i8 111, i8 114, i8 109, i8 97, i8 116, i8 105, i8 99, i8 115, i8 0]

@string_literal_11 = global [15 x i8] [i8 66, i8 105, i8 111, i8 105, i8 110, i8 102, i8 111, i8 114, i8 109, i8 97, i8 116, i8 105, i8 99, i8 115, i8 0]

@string_literal_12 = global [57 x i8] [i8 77, i8 101, i8 116, i8 104, i8 111, i8 100, i8 115, i8 32, i8 97, i8 110, i8 100, i8 32, i8 115, i8 111, i8 102, i8 116, i8 119, i8 97, i8 114, i8 101, i8 32, i8 102, i8 111, i8 114, i8 32, i8 116, i8 104, i8 101, i8 32, i8 97, i8 110, i8 97, i8 108, i8 121, i8 115, i8 105, i8 115, i8 32, i8 111, i8 102, i8 32, i8 98, i8 105, i8 111, i8 108, i8 111, i8 103, i8 105, i8 99, i8 97, i8 108, i8 32, i8 100, i8 97, i8 116, i8 97, i8 0]

@string_literal_13 = global [6 x i8] [i8 99, i8 108, i8 111, i8 117, i8 100, i8 0]

@string_literal_14 = global [16 x i8] [i8 67, i8 108, i8 111, i8 117, i8 100, i8 32, i8 67, i8 111, i8 109, i8 112, i8 117, i8 116, i8 105, i8 110, i8 103, i8 0]

@string_literal_15 = global [38 x i8] [i8 66, i8 105, i8 110, i8 100, i8 105, i8 110, i8 103, i8 115, i8 32, i8 116, i8 111, i8 32, i8 67, i8 108, i8 111, i8 117, i8 100, i8 32, i8 67, i8 111, i8 109, i8 112, i8 117, i8 116, i8 105, i8 110, i8 103, i8 32, i8 112, i8 108, i8 97, i8 116, i8 102, i8 111, i8 114, i8 109, i8 115, i8 0]

@string_literal_16 = global [13 x i8] [i8 99, i8 111, i8 109, i8 109, i8 97, i8 110, i8 100, i8 45, i8 108, i8 105, i8 110, i8 101, i8 0]

@string_literal_17 = global [18 x i8] [i8 67, i8 76, i8 73, i8 32, i8 38, i8 32, i8 84, i8 85, i8 73, i8 32, i8 116, i8 111, i8 111, i8 108, i8 105, i8 110, i8 103, i8 0]

@string_literal_18 = global [46 x i8] [i8 76, i8 105, i8 98, i8 114, i8 97, i8 114, i8 105, i8 101, i8 115, i8 32, i8 116, i8 111, i8 32, i8 100, i8 101, i8 118, i8 101, i8 108, i8 111, i8 112, i8 101, i8 32, i8 99, i8 111, i8 109, i8 109, i8 97, i8 110, i8 100, i8 45, i8 108, i8 105, i8 110, i8 101, i8 32, i8 105, i8 110, i8 116, i8 101, i8 114, i8 102, i8 97, i8 99, i8 101, i8 115, i8 0]

@string_literal_19 = global [23 x i8] [i8 99, i8 111, i8 109, i8 112, i8 105, i8 108, i8 101, i8 114, i8 115, i8 45, i8 105, i8 110, i8 116, i8 101, i8 114, i8 112, i8 114, i8 101, i8 116, i8 101, i8 114, i8 115, i8 0]

@string_literal_20 = global [27 x i8] [i8 67, i8 111, i8 109, i8 112, i8 105, i8 108, i8 101, i8 114, i8 115, i8 32, i8 97, i8 110, i8 100, i8 32, i8 73, i8 110, i8 116, i8 101, i8 114, i8 112, i8 114, i8 101, i8 116, i8 101, i8 114, i8 115, i8 0]

@string_literal_21 = global [45 x i8] [i8 84, i8 111, i8 111, i8 108, i8 105, i8 110, i8 103, i8 32, i8 116, i8 111, i8 32, i8 99, i8 114, i8 101, i8 97, i8 116, i8 101, i8 32, i8 99, i8 111, i8 109, i8 112, i8 105, i8 108, i8 101, i8 114, i8 115, i8 32, i8 97, i8 110, i8 100, i8 32, i8 105, i8 110, i8 116, i8 101, i8 114, i8 112, i8 114, i8 101, i8 116, i8 101, i8 114, i8 115, i8 0]

@string_literal_22 = global [12 x i8] [i8 99, i8 111, i8 109, i8 112, i8 114, i8 101, i8 115, i8 115, i8 105, i8 111, i8 110, i8 0]

@string_literal_23 = global [17 x i8] [i8 68, i8 97, i8 116, i8 97, i8 32, i8 99, i8 111, i8 109, i8 112, i8 114, i8 101, i8 115, i8 115, i8 105, i8 111, i8 110, i8 0]

@string_literal_24 = global [28 x i8] [i8 82, i8 101, i8 100, i8 117, i8 99, i8 105, i8 110, i8 103, i8 32, i8 116, i8 104, i8 101, i8 32, i8 115, i8 105, i8 122, i8 101, i8 32, i8 111, i8 102, i8 32, i8 116, i8 104, i8 105, i8 110, i8 103, i8 115, i8 0]

@string_literal_25 = global [12 x i8] [i8 99, i8 111, i8 110, i8 99, i8 117, i8 114, i8 114, i8 101, i8 110, i8 99, i8 121, i8 0]

@string_literal_26 = global [12 x i8] [i8 67, i8 111, i8 110, i8 99, i8 117, i8 114, i8 114, i8 101, i8 110, i8 99, i8 121, i8 0]

@string_literal_27 = global [44 x i8] [i8 67, i8 111, i8 110, i8 99, i8 117, i8 114, i8 114, i8 101, i8 110, i8 116, i8 32, i8 112, i8 114, i8 111, i8 103, i8 114, i8 97, i8 109, i8 109, i8 105, i8 110, i8 103, i8 32, i8 116, i8 101, i8 99, i8 104, i8 110, i8 105, i8 113, i8 117, i8 101, i8 115, i8 32, i8 97, i8 110, i8 100, i8 32, i8 116, i8 111, i8 111, i8 108, i8 115, i8 0]

@string_literal_28 = global [13 x i8] [i8 99, i8 114, i8 121, i8 112, i8 116, i8 111, i8 103, i8 114, i8 97, i8 112, i8 104, i8 121, i8 0]

@string_literal_29 = global [13 x i8] [i8 67, i8 114, i8 121, i8 112, i8 116, i8 111, i8 103, i8 114, i8 97, i8 112, i8 104, i8 121, i8 0]

@string_literal_30 = global [43 x i8] [i8 65, i8 108, i8 103, i8 111, i8 114, i8 105, i8 116, i8 104, i8 109, i8 115, i8 32, i8 102, i8 111, i8 114, i8 32, i8 101, i8 110, i8 99, i8 114, i8 121, i8 112, i8 116, i8 105, i8 110, i8 103, i8 32, i8 97, i8 110, i8 100, i8 32, i8 104, i8 97, i8 115, i8 104, i8 105, i8 110, i8 103, i8 32, i8 100, i8 97, i8 116, i8 97, i8 0]

@string_literal_31 = global [16 x i8] [i8 100, i8 97, i8 116, i8 97, i8 45, i8 115, i8 116, i8 114, i8 117, i8 99, i8 116, i8 117, i8 114, i8 101, i8 115, i8 0]

@string_literal_32 = global [16 x i8] [i8 68, i8 97, i8 116, i8 97, i8 32, i8 83, i8 116, i8 114, i8 117, i8 99, i8 116, i8 117, i8 114, i8 101, i8 115, i8 0]

@string_literal_33 = global [54 x i8] [i8 68, i8 97, i8 116, i8 97, i8 32, i8 115, i8 116, i8 114, i8 117, i8 99, i8 116, i8 117, i8 114, i8 101, i8 115, i8 44, i8 32, i8 119, i8 104, i8 101, i8 116, i8 104, i8 101, i8 114, i8 32, i8 112, i8 117, i8 114, i8 101, i8 108, i8 121, i8 32, i8 102, i8 117, i8 110, i8 99, i8 116, i8 105, i8 111, i8 110, i8 97, i8 108, i8 32, i8 111, i8 114, i8 32, i8 109, i8 117, i8 116, i8 97, i8 98, i8 108, i8 101, i8 0]

@string_literal_34 = global [10 x i8] [i8 100, i8 97, i8 116, i8 97, i8 98, i8 97, i8 115, i8 101, i8 115, i8 0]

@string_literal_35 = global [10 x i8] [i8 68, i8 97, i8 116, i8 97, i8 98, i8 97, i8 115, i8 101, i8 115, i8 0]

@string_literal_36 = global [32 x i8] [i8 68, i8 97, i8 116, i8 97, i8 98, i8 97, i8 115, i8 101, i8 32, i8 100, i8 114, i8 105, i8 118, i8 101, i8 114, i8 115, i8 32, i8 97, i8 110, i8 100, i8 32, i8 105, i8 110, i8 116, i8 101, i8 114, i8 102, i8 97, i8 99, i8 101, i8 115, i8 0]

@string_literal_37 = global [12 x i8] [i8 100, i8 101, i8 118, i8 101, i8 108, i8 111, i8 112, i8 109, i8 101, i8 110, i8 116, i8 0]

@string_literal_38 = global [12 x i8] [i8 68, i8 101, i8 118, i8 101, i8 108, i8 111, i8 112, i8 109, i8 101, i8 110, i8 116, i8 0]

@string_literal_39 = global [54 x i8] [i8 68, i8 101, i8 118, i8 101, i8 108, i8 111, i8 112, i8 109, i8 101, i8 110, i8 116, i8 32, i8 104, i8 101, i8 108, i8 112, i8 101, i8 114, i8 115, i8 44, i8 32, i8 105, i8 110, i8 116, i8 101, i8 103, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 111, i8 116, i8 104, i8 101, i8 114, i8 32, i8 108, i8 97, i8 110, i8 103, i8 117, i8 97, i8 103, i8 101, i8 115, i8 0]

@string_literal_40 = global [12 x i8] [i8 100, i8 105, i8 115, i8 116, i8 114, i8 105, i8 98, i8 117, i8 116, i8 101, i8 100, i8 0]

@string_literal_41 = global [34 x i8] [i8 68, i8 105, i8 115, i8 116, i8 114, i8 105, i8 98, i8 117, i8 116, i8 101, i8 100, i8 32, i8 83, i8 121, i8 115, i8 116, i8 101, i8 109, i8 115, i8 32, i8 38, i8 32, i8 67, i8 111, i8 109, i8 112, i8 117, i8 116, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_42 = global [55 x i8] [i8 84, i8 111, i8 111, i8 108, i8 105, i8 110, i8 103, i8 32, i8 97, i8 110, i8 100, i8 32, i8 116, i8 101, i8 99, i8 104, i8 110, i8 105, i8 113, i8 117, i8 101, i8 115, i8 32, i8 102, i8 111, i8 114, i8 32, i8 119, i8 114, i8 105, i8 116, i8 105, i8 110, i8 103, i8 32, i8 100, i8 105, i8 115, i8 116, i8 114, i8 105, i8 98, i8 117, i8 116, i8 101, i8 100, i8 32, i8 115, i8 121, i8 115, i8 116, i8 101, i8 109, i8 115, i8 0]

@string_literal_43 = global [13 x i8] [i8 100, i8 105, i8 115, i8 116, i8 114, i8 105, i8 98, i8 117, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_44 = global [21 x i8] [i8 80, i8 97, i8 99, i8 107, i8 97, i8 103, i8 101, i8 32, i8 68, i8 105, i8 115, i8 116, i8 114, i8 105, i8 98, i8 117, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_45 = global [57 x i8] [i8 66, i8 117, i8 105, i8 108, i8 100, i8 105, i8 110, i8 103, i8 44, i8 32, i8 80, i8 97, i8 99, i8 107, i8 97, i8 103, i8 105, i8 110, i8 103, i8 32, i8 97, i8 110, i8 100, i8 32, i8 68, i8 105, i8 115, i8 116, i8 114, i8 105, i8 98, i8 117, i8 116, i8 105, i8 110, i8 103, i8 32, i8 115, i8 111, i8 102, i8 116, i8 119, i8 97, i8 114, i8 101, i8 32, i8 105, i8 110, i8 32, i8 72, i8 97, i8 115, i8 107, i8 101, i8 108, i8 108, i8 0]

@string_literal_46 = global [4 x i8] [i8 102, i8 102, i8 105, i8 0]

@string_literal_47 = global [4 x i8] [i8 70, i8 70, i8 73, i8 0]

@string_literal_48 = global [53 x i8] [i8 87, i8 111, i8 114, i8 107, i8 105, i8 110, i8 103, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 111, i8 116, i8 104, i8 101, i8 114, i8 32, i8 108, i8 97, i8 110, i8 103, i8 117, i8 97, i8 103, i8 101, i8 115, i8 32, i8 97, i8 110, i8 100, i8 32, i8 103, i8 101, i8 110, i8 101, i8 114, i8 97, i8 116, i8 105, i8 110, i8 103, i8 32, i8 98, i8 105, i8 110, i8 100, i8 105, i8 110, i8 103, i8 115, i8 0]

@string_literal_49 = global [4 x i8] [i8 102, i8 114, i8 112, i8 0]

@string_literal_50 = global [4 x i8] [i8 70, i8 82, i8 80, i8 0]

@string_literal_51 = global [32 x i8] [i8 70, i8 117, i8 110, i8 99, i8 116, i8 105, i8 111, i8 110, i8 97, i8 108, i8 32, i8 82, i8 101, i8 97, i8 99, i8 116, i8 105, i8 118, i8 101, i8 32, i8 80, i8 114, i8 111, i8 103, i8 114, i8 97, i8 109, i8 109, i8 105, i8 110, i8 103, i8 0]

@string_literal_52 = global [9 x i8] [i8 103, i8 97, i8 109, i8 101, i8 45, i8 100, i8 101, i8 118, i8 0]

@string_literal_53 = global [17 x i8] [i8 71, i8 97, i8 109, i8 101, i8 32, i8 68, i8 101, i8 118, i8 101, i8 108, i8 111, i8 112, i8 109, i8 101, i8 110, i8 116, i8 0]

@string_literal_54 = global [36 x i8] [i8 76, i8 105, i8 98, i8 114, i8 97, i8 114, i8 105, i8 101, i8 115, i8 32, i8 117, i8 115, i8 101, i8 100, i8 32, i8 102, i8 111, i8 114, i8 32, i8 103, i8 97, i8 109, i8 101, i8 32, i8 100, i8 101, i8 118, i8 101, i8 108, i8 111, i8 112, i8 109, i8 101, i8 110, i8 116, i8 0]

@string_literal_55 = global [9 x i8] [i8 103, i8 101, i8 110, i8 101, i8 114, i8 105, i8 99, i8 115, i8 0]

@string_literal_56 = global [9 x i8] [i8 71, i8 101, i8 110, i8 101, i8 114, i8 105, i8 99, i8 115, i8 0]

@string_literal_57 = global [42 x i8] [i8 87, i8 111, i8 114, i8 107, i8 105, i8 110, i8 103, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 72, i8 97, i8 115, i8 107, i8 101, i8 108, i8 108, i8 39, i8 115, i8 32, i8 71, i8 101, i8 110, i8 101, i8 114, i8 105, i8 99, i8 115, i8 32, i8 109, i8 101, i8 99, i8 104, i8 97, i8 110, i8 105, i8 115, i8 109, i8 0]

@string_literal_58 = global [9 x i8] [i8 103, i8 114, i8 97, i8 112, i8 104, i8 105, i8 99, i8 115, i8 0]

@string_literal_59 = global [9 x i8] [i8 71, i8 114, i8 97, i8 112, i8 104, i8 105, i8 99, i8 115, i8 0]

@string_literal_60 = global [40 x i8] [i8 80, i8 114, i8 111, i8 103, i8 114, i8 97, i8 109, i8 109, i8 105, i8 110, i8 103, i8 32, i8 116, i8 104, i8 101, i8 32, i8 115, i8 121, i8 115, i8 116, i8 101, i8 109, i8 39, i8 115, i8 32, i8 114, i8 101, i8 110, i8 100, i8 101, i8 114, i8 105, i8 110, i8 103, i8 32, i8 65, i8 80, i8 73, i8 115, i8 0]

@string_literal_61 = global [4 x i8] [i8 103, i8 117, i8 105, i8 0]

@string_literal_62 = global [4 x i8] [i8 71, i8 85, i8 73, i8 0]

@string_literal_63 = global [35 x i8] [i8 67, i8 114, i8 101, i8 97, i8 116, i8 105, i8 110, i8 103, i8 32, i8 103, i8 114, i8 97, i8 112, i8 104, i8 105, i8 99, i8 97, i8 108, i8 32, i8 117, i8 115, i8 101, i8 114, i8 32, i8 105, i8 110, i8 116, i8 101, i8 114, i8 102, i8 97, i8 99, i8 101, i8 115, i8 0]

@string_literal_64 = global [9 x i8] [i8 104, i8 97, i8 114, i8 100, i8 119, i8 97, i8 114, i8 101, i8 0]

@string_literal_65 = global [9 x i8] [i8 72, i8 97, i8 114, i8 100, i8 119, i8 97, i8 114, i8 101, i8 0]

@string_literal_66 = global [53 x i8] [i8 68, i8 105, i8 103, i8 105, i8 116, i8 97, i8 108, i8 32, i8 99, i8 105, i8 114, i8 99, i8 117, i8 105, i8 116, i8 32, i8 100, i8 101, i8 115, i8 99, i8 114, i8 105, i8 112, i8 116, i8 105, i8 111, i8 110, i8 32, i8 97, i8 110, i8 100, i8 32, i8 104, i8 97, i8 114, i8 100, i8 119, i8 97, i8 114, i8 101, i8 32, i8 105, i8 110, i8 116, i8 101, i8 114, i8 102, i8 97, i8 99, i8 105, i8 110, i8 103, i8 0]

@string_literal_67 = global [5 x i8] [i8 106, i8 115, i8 111, i8 110, i8 0]

@string_literal_68 = global [5 x i8] [i8 74, i8 83, i8 79, i8 78, i8 0]

@string_literal_69 = global [41 x i8] [i8 80, i8 97, i8 114, i8 115, i8 105, i8 110, i8 103, i8 44, i8 32, i8 112, i8 114, i8 111, i8 100, i8 117, i8 99, i8 105, i8 110, i8 103, i8 32, i8 97, i8 110, i8 100, i8 32, i8 109, i8 97, i8 110, i8 105, i8 112, i8 117, i8 108, i8 97, i8 116, i8 105, i8 110, i8 103, i8 32, i8 74, i8 83, i8 79, i8 78, i8 0]

@string_literal_70 = global [9 x i8] [i8 108, i8 97, i8 110, i8 103, i8 117, i8 97, i8 103, i8 101, i8 0]

@string_literal_71 = global [9 x i8] [i8 76, i8 97, i8 110, i8 103, i8 117, i8 97, i8 103, i8 101, i8 0]

@string_literal_72 = global [58 x i8] [i8 73, i8 110, i8 116, i8 101, i8 114, i8 102, i8 97, i8 99, i8 105, i8 110, i8 103, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 111, i8 116, i8 104, i8 101, i8 114, i8 32, i8 112, i8 114, i8 111, i8 103, i8 114, i8 97, i8 109, i8 109, i8 105, i8 110, i8 103, i8 32, i8 108, i8 97, i8 110, i8 103, i8 117, i8 97, i8 103, i8 101, i8 115, i8 32, i8 102, i8 114, i8 111, i8 109, i8 32, i8 72, i8 97, i8 115, i8 107, i8 101, i8 108, i8 108, i8 0]

@string_literal_73 = global [7 x i8] [i8 108, i8 101, i8 110, i8 115, i8 101, i8 115, i8 0]

@string_literal_74 = global [7 x i8] [i8 76, i8 101, i8 110, i8 115, i8 101, i8 115, i8 0]

@string_literal_75 = global [59 x i8] [i8 70, i8 117, i8 110, i8 99, i8 116, i8 105, i8 111, i8 110, i8 97, i8 108, i8 32, i8 114, i8 101, i8 102, i8 101, i8 114, i8 101, i8 110, i8 99, i8 101, i8 115, i8 32, i8 115, i8 117, i8 99, i8 104, i8 32, i8 97, i8 115, i8 32, i8 76, i8 101, i8 110, i8 115, i8 101, i8 115, i8 44, i8 32, i8 70, i8 111, i8 108, i8 100, i8 115, i8 32, i8 97, i8 110, i8 100, i8 32, i8 84, i8 114, i8 97, i8 118, i8 101, i8 114, i8 115, i8 97, i8 108, i8 115, i8 0]

@string_literal_76 = global [6 x i8] [i8 109, i8 97, i8 116, i8 104, i8 115, i8 0]

@string_literal_77 = global [12 x i8] [i8 77, i8 97, i8 116, i8 104, i8 101, i8 109, i8 97, i8 116, i8 105, i8 99, i8 115, i8 0]

@string_literal_78 = global [36 x i8] [i8 78, i8 117, i8 109, i8 101, i8 114, i8 105, i8 99, i8 97, i8 108, i8 32, i8 97, i8 110, i8 100, i8 32, i8 77, i8 97, i8 116, i8 104, i8 101, i8 109, i8 97, i8 116, i8 105, i8 99, i8 97, i8 108, i8 32, i8 112, i8 97, i8 99, i8 107, i8 97, i8 103, i8 101, i8 115, i8 0]

@string_literal_79 = global [7 x i8] [i8 109, i8 111, i8 110, i8 97, i8 100, i8 115, i8 0]

@string_literal_80 = global [7 x i8] [i8 77, i8 111, i8 110, i8 97, i8 100, i8 115, i8 0]

@string_literal_81 = global [34 x i8] [i8 69, i8 102, i8 102, i8 101, i8 99, i8 116, i8 102, i8 117, i8 108, i8 32, i8 115, i8 101, i8 113, i8 117, i8 101, i8 110, i8 116, i8 105, i8 97, i8 108, i8 32, i8 99, i8 111, i8 109, i8 112, i8 117, i8 116, i8 97, i8 116, i8 105, i8 111, i8 110, i8 115, i8 0]

@string_literal_82 = global [8 x i8] [i8 110, i8 101, i8 116, i8 119, i8 111, i8 114, i8 107, i8 0]

@string_literal_83 = global [20 x i8] [i8 78, i8 101, i8 116, i8 119, i8 111, i8 114, i8 107, i8 32, i8 68, i8 101, i8 118, i8 101, i8 108, i8 111, i8 112, i8 109, i8 101, i8 110, i8 116, i8 0]

@string_literal_84 = global [63 x i8] [i8 67, i8 111, i8 110, i8 110, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 32, i8 112, i8 111, i8 111, i8 108, i8 115, i8 44, i8 32, i8 68, i8 78, i8 83, i8 44, i8 32, i8 72, i8 84, i8 84, i8 80, i8 44, i8 32, i8 65, i8 80, i8 73, i8 32, i8 99, i8 108, i8 105, i8 101, i8 110, i8 116, i8 115, i8 32, i8 97, i8 110, i8 100, i8 32, i8 110, i8 101, i8 116, i8 119, i8 111, i8 114, i8 107, i8 32, i8 112, i8 114, i8 111, i8 116, i8 111, i8 99, i8 111, i8 108, i8 115, i8 0]

@string_literal_85 = global [4 x i8] [i8 110, i8 108, i8 112, i8 0]

@string_literal_86 = global [28 x i8] [i8 78, i8 97, i8 116, i8 117, i8 114, i8 97, i8 108, i8 32, i8 76, i8 97, i8 110, i8 103, i8 117, i8 97, i8 103, i8 101, i8 32, i8 80, i8 114, i8 111, i8 99, i8 101, i8 115, i8 115, i8 105, i8 110, i8 103, i8 0]

@string_literal_87 = global [39 x i8] [i8 84, i8 111, i8 111, i8 108, i8 105, i8 110, i8 103, i8 32, i8 116, i8 111, i8 32, i8 119, i8 111, i8 114, i8 107, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 110, i8 97, i8 116, i8 117, i8 114, i8 97, i8 108, i8 32, i8 108, i8 97, i8 110, i8 103, i8 117, i8 97, i8 103, i8 101, i8 115, i8 0]

@string_literal_88 = global [12 x i8] [i8 112, i8 97, i8 114, i8 97, i8 108, i8 108, i8 101, i8 108, i8 105, i8 115, i8 109, i8 0]

@string_literal_89 = global [12 x i8] [i8 80, i8 97, i8 114, i8 97, i8 108, i8 108, i8 101, i8 108, i8 105, i8 115, i8 109, i8 0]

@string_literal_90 = global [21 x i8] [i8 80, i8 97, i8 114, i8 97, i8 108, i8 108, i8 101, i8 108, i8 32, i8 112, i8 114, i8 111, i8 103, i8 114, i8 97, i8 109, i8 109, i8 105, i8 110, i8 103, i8 0]

@string_literal_91 = global [23 x i8] [i8 112, i8 97, i8 114, i8 115, i8 101, i8 114, i8 45, i8 105, i8 109, i8 112, i8 108, i8 101, i8 109, i8 101, i8 110, i8 116, i8 97, i8 116, i8 105, i8 111, i8 110, i8 115, i8 0]

@string_literal_92 = global [23 x i8] [i8 80, i8 97, i8 114, i8 115, i8 101, i8 114, i8 32, i8 73, i8 109, i8 112, i8 108, i8 101, i8 109, i8 101, i8 110, i8 116, i8 97, i8 116, i8 105, i8 111, i8 110, i8 115, i8 0]

@string_literal_93 = global [21 x i8] [i8 80, i8 97, i8 114, i8 115, i8 105, i8 110, i8 103, i8 32, i8 100, i8 97, i8 116, i8 97, i8 32, i8 102, i8 111, i8 114, i8 109, i8 97, i8 116, i8 115, i8 0]

@string_literal_94 = global [8 x i8] [i8 112, i8 97, i8 114, i8 115, i8 101, i8 114, i8 115, i8 0]

@string_literal_95 = global [8 x i8] [i8 80, i8 97, i8 114, i8 115, i8 101, i8 114, i8 115, i8 0]

@string_literal_96 = global [35 x i8] [i8 76, i8 105, i8 98, i8 114, i8 97, i8 114, i8 105, i8 101, i8 115, i8 32, i8 116, i8 111, i8 32, i8 105, i8 110, i8 103, i8 101, i8 115, i8 116, i8 32, i8 97, i8 110, i8 100, i8 32, i8 112, i8 97, i8 114, i8 115, i8 101, i8 32, i8 100, i8 97, i8 116, i8 97, i8 0]

@string_literal_97 = global [8 x i8] [i8 112, i8 97, i8 114, i8 115, i8 105, i8 110, i8 103, i8 0]

@string_literal_98 = global [8 x i8] [i8 80, i8 97, i8 114, i8 115, i8 105, i8 110, i8 103, i8 0]

@string_literal_99 = global [62 x i8] [i8 80, i8 97, i8 114, i8 115, i8 101, i8 114, i8 32, i8 103, i8 101, i8 110, i8 101, i8 114, i8 97, i8 116, i8 111, i8 114, i8 115, i8 44, i8 32, i8 99, i8 111, i8 109, i8 98, i8 105, i8 110, i8 97, i8 116, i8 111, i8 114, i8 115, i8 32, i8 97, i8 110, i8 100, i8 32, i8 116, i8 111, i8 111, i8 108, i8 115, i8 32, i8 116, i8 111, i8 32, i8 104, i8 101, i8 108, i8 112, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 112, i8 97, i8 114, i8 115, i8 105, i8 110, i8 103, i8 0]

@string_literal_100 = global [8 x i8] [i8 112, i8 104, i8 121, i8 115, i8 105, i8 99, i8 115, i8 0]

@string_literal_101 = global [8 x i8] [i8 80, i8 104, i8 121, i8 115, i8 105, i8 99, i8 115, i8 0]

@string_literal_102 = global [60 x i8] [i8 84, i8 104, i8 101, i8 32, i8 115, i8 116, i8 117, i8 100, i8 121, i8 32, i8 111, i8 102, i8 32, i8 109, i8 97, i8 116, i8 116, i8 101, i8 114, i8 44, i8 32, i8 105, i8 116, i8 115, i8 32, i8 99, i8 111, i8 110, i8 115, i8 105, i8 116, i8 117, i8 101, i8 110, i8 116, i8 115, i8 44, i8 32, i8 109, i8 111, i8 116, i8 105, i8 111, i8 110, i8 44, i8 32, i8 97, i8 110, i8 100, i8 32, i8 98, i8 101, i8 104, i8 97, i8 118, i8 105, i8 111, i8 117, i8 114, i8 0]

@string_literal_103 = global [8 x i8] [i8 112, i8 114, i8 101, i8 108, i8 117, i8 100, i8 101, i8 0]

@string_literal_104 = global [8 x i8] [i8 80, i8 114, i8 101, i8 108, i8 117, i8 100, i8 101, i8 0]

@string_literal_105 = global [39 x i8] [i8 76, i8 105, i8 98, i8 114, i8 97, i8 114, i8 105, i8 101, i8 115, i8 32, i8 116, i8 104, i8 97, i8 116, i8 32, i8 112, i8 114, i8 111, i8 118, i8 105, i8 100, i8 101, i8 32, i8 100, i8 101, i8 102, i8 97, i8 117, i8 108, i8 116, i8 32, i8 105, i8 109, i8 112, i8 111, i8 114, i8 116, i8 115, i8 0]

@string_literal_106 = global [10 x i8] [i8 112, i8 114, i8 111, i8 102, i8 105, i8 108, i8 105, i8 110, i8 103, i8 0]

@string_literal_107 = global [10 x i8] [i8 80, i8 114, i8 111, i8 102, i8 105, i8 108, i8 105, i8 110, i8 103, i8 0]

@string_literal_108 = global [39 x i8] [i8 77, i8 101, i8 97, i8 115, i8 117, i8 114, i8 101, i8 32, i8 116, i8 104, i8 101, i8 32, i8 98, i8 101, i8 104, i8 97, i8 118, i8 105, i8 111, i8 117, i8 114, i8 32, i8 111, i8 102, i8 32, i8 121, i8 111, i8 117, i8 114, i8 32, i8 112, i8 114, i8 111, i8 103, i8 114, i8 97, i8 109, i8 115, i8 0]

@string_literal_109 = global [10 x i8] [i8 115, i8 116, i8 114, i8 101, i8 97, i8 109, i8 105, i8 110, i8 103, i8 0]

@string_literal_110 = global [10 x i8] [i8 83, i8 116, i8 114, i8 101, i8 97, i8 109, i8 105, i8 110, i8 103, i8 0]

@string_literal_111 = global [41 x i8] [i8 68, i8 97, i8 116, i8 97, i8 32, i8 115, i8 116, i8 114, i8 101, i8 97, i8 109, i8 105, i8 110, i8 103, i8 32, i8 102, i8 111, i8 114, i8 32, i8 99, i8 111, i8 110, i8 116, i8 105, i8 110, i8 117, i8 111, i8 117, i8 115, i8 32, i8 112, i8 114, i8 111, i8 99, i8 101, i8 115, i8 115, i8 105, i8 110, i8 103, i8 0]

@string_literal_112 = global [7 x i8] [i8 115, i8 121, i8 115, i8 116, i8 101, i8 109, i8 0]

@string_literal_113 = global [20 x i8] [i8 83, i8 121, i8 115, i8 116, i8 101, i8 109, i8 115, i8 32, i8 80, i8 114, i8 111, i8 103, i8 114, i8 97, i8 109, i8 109, i8 105, i8 110, i8 103, i8 0]

@string_literal_114 = global [56 x i8] [i8 80, i8 114, i8 111, i8 103, i8 114, i8 97, i8 109, i8 109, i8 105, i8 110, i8 103, i8 32, i8 97, i8 110, i8 100, i8 32, i8 99, i8 111, i8 109, i8 109, i8 117, i8 110, i8 105, i8 99, i8 97, i8 116, i8 105, i8 110, i8 103, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 116, i8 104, i8 101, i8 32, i8 79, i8 112, i8 101, i8 114, i8 97, i8 116, i8 105, i8 110, i8 103, i8 32, i8 83, i8 121, i8 115, i8 116, i8 101, i8 109, i8 0]

@string_literal_115 = global [10 x i8] [i8 116, i8 101, i8 108, i8 101, i8 109, i8 101, i8 116, i8 114, i8 121, i8 0]

@string_literal_116 = global [10 x i8] [i8 84, i8 101, i8 108, i8 101, i8 109, i8 101, i8 116, i8 114, i8 121, i8 0]

@string_literal_117 = global [22 x i8] [i8 83, i8 121, i8 115, i8 116, i8 101, i8 109, i8 115, i8 32, i8 79, i8 98, i8 115, i8 101, i8 114, i8 118, i8 97, i8 98, i8 105, i8 108, i8 105, i8 116, i8 121, i8 0]

@string_literal_118 = global [17 x i8] [i8 116, i8 101, i8 109, i8 112, i8 108, i8 97, i8 116, i8 101, i8 45, i8 104, i8 97, i8 115, i8 107, i8 101, i8 108, i8 108, i8 0]

@string_literal_119 = global [17 x i8] [i8 84, i8 101, i8 109, i8 112, i8 108, i8 97, i8 116, i8 101, i8 32, i8 72, i8 97, i8 115, i8 107, i8 101, i8 108, i8 108, i8 0]

@string_literal_120 = global [38 x i8] [i8 77, i8 101, i8 116, i8 97, i8 112, i8 114, i8 111, i8 103, i8 114, i8 97, i8 109, i8 109, i8 105, i8 110, i8 103, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 84, i8 101, i8 109, i8 112, i8 108, i8 97, i8 116, i8 101, i8 32, i8 72, i8 97, i8 115, i8 107, i8 101, i8 108, i8 108, i8 0]

@string_literal_121 = global [8 x i8] [i8 116, i8 101, i8 115, i8 116, i8 105, i8 110, i8 103, i8 0]

@string_literal_122 = global [8 x i8] [i8 84, i8 101, i8 115, i8 116, i8 105, i8 110, i8 103, i8 0]

@string_literal_123 = global [16 x i8] [i8 84, i8 101, i8 115, i8 116, i8 32, i8 102, i8 114, i8 97, i8 109, i8 101, i8 119, i8 111, i8 114, i8 107, i8 115, i8 0]

@string_literal_124 = global [5 x i8] [i8 116, i8 101, i8 120, i8 116, i8 0]

@string_literal_125 = global [5 x i8] [i8 84, i8 101, i8 120, i8 116, i8 0]

@string_literal_126 = global [41 x i8] [i8 87, i8 111, i8 114, i8 107, i8 105, i8 110, i8 103, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 116, i8 101, i8 120, i8 116, i8 117, i8 97, i8 108, i8 32, i8 100, i8 97, i8 116, i8 97, i8 32, i8 97, i8 110, i8 100, i8 32, i8 97, i8 108, i8 103, i8 111, i8 114, i8 105, i8 116, i8 104, i8 109, i8 115, i8 0]

@string_literal_127 = global [12 x i8] [i8 116, i8 121, i8 112, i8 101, i8 45, i8 115, i8 121, i8 115, i8 116, i8 101, i8 109, i8 0]

@string_literal_128 = global [12 x i8] [i8 84, i8 121, i8 112, i8 101, i8 32, i8 83, i8 121, i8 115, i8 116, i8 101, i8 109, i8 0]

@string_literal_129 = global [34 x i8] [i8 69, i8 110, i8 104, i8 97, i8 110, i8 99, i8 105, i8 110, i8 103, i8 32, i8 116, i8 104, i8 101, i8 32, i8 72, i8 97, i8 115, i8 107, i8 101, i8 108, i8 108, i8 32, i8 116, i8 121, i8 112, i8 101, i8 32, i8 115, i8 121, i8 115, i8 116, i8 101, i8 109, i8 0]

@string_literal_130 = global [4 x i8] [i8 119, i8 101, i8 98, i8 0]

@string_literal_131 = global [16 x i8] [i8 87, i8 101, i8 98, i8 32, i8 68, i8 101, i8 118, i8 101, i8 108, i8 111, i8 112, i8 109, i8 101, i8 110, i8 116, i8 0]

@string_literal_132 = global [24 x i8] [i8 80, i8 114, i8 111, i8 103, i8 114, i8 97, i8 109, i8 109, i8 105, i8 110, i8 103, i8 32, i8 102, i8 111, i8 114, i8 32, i8 116, i8 104, i8 101, i8 32, i8 119, i8 101, i8 98, i8 0]

@string_literal_133 = global [4 x i8] [i8 120, i8 109, i8 108, i8 0]

@string_literal_134 = global [4 x i8] [i8 88, i8 77, i8 76, i8 0]

@string_literal_135 = global [47 x i8] [i8 76, i8 105, i8 98, i8 114, i8 97, i8 114, i8 105, i8 101, i8 115, i8 32, i8 116, i8 111, i8 32, i8 99, i8 111, i8 110, i8 115, i8 117, i8 109, i8 101, i8 32, i8 97, i8 110, i8 100, i8 32, i8 112, i8 114, i8 111, i8 100, i8 117, i8 99, i8 101, i8 32, i8 88, i8 77, i8 76, i8 32, i8 100, i8 111, i8 99, i8 117, i8 109, i8 101, i8 110, i8 116, i8 115, i8 0]

@string_literal_136 = global [19 x i8] [i8 110, i8 111, i8 114, i8 109, i8 97, i8 108, i8 105, i8 115, i8 101, i8 95, i8 99, i8 97, i8 116, i8 101, i8 103, i8 111, i8 114, i8 121, i8 0]

@string_literal_137 = global [10 x i8] [i8 65, i8 108, i8 103, i8 111, i8 114, i8 105, i8 116, i8 104, i8 109, i8 0]

@string_literal_138 = global [7 x i8] [i8 67, i8 114, i8 121, i8 112, i8 116, i8 111, i8 0]

@string_literal_139 = global [4 x i8] [i8 67, i8 76, i8 73, i8 0]

@string_literal_140 = global [22 x i8] [i8 67, i8 76, i8 73, i8 32, i8 38, i8 32, i8 84, i8 85, i8 73, i8 32, i8 68, i8 101, i8 118, i8 101, i8 108, i8 111, i8 112, i8 109, i8 101, i8 110, i8 116, i8 0]

@string_literal_141 = global [4 x i8] [i8 84, i8 85, i8 73, i8 0]

@string_literal_142 = global [13 x i8] [i8 67, i8 111, i8 109, i8 109, i8 97, i8 110, i8 100, i8 32, i8 76, i8 105, i8 110, i8 101, i8 0]

@string_literal_143 = global [12 x i8] [i8 67, i8 111, i8 109, i8 109, i8 97, i8 110, i8 100, i8 76, i8 105, i8 110, i8 101, i8 0]

@string_literal_144 = global [8 x i8] [i8 78, i8 117, i8 109, i8 101, i8 114, i8 105, i8 99, i8 0]

@string_literal_145 = global [10 x i8] [i8 78, i8 117, i8 109, i8 101, i8 114, i8 105, i8 99, i8 97, i8 108, i8 0]

@string_literal_146 = global [9 x i8] [i8 78, i8 117, i8 109, i8 101, i8 114, i8 105, i8 99, i8 115, i8 0]

@string_literal_147 = global [11 x i8] [i8 65, i8 114, i8 105, i8 116, i8 104, i8 109, i8 101, i8 116, i8 105, i8 99, i8 0]

@string_literal_148 = global [14 x i8] [i8 78, i8 117, i8 109, i8 98, i8 101, i8 114, i8 32, i8 84, i8 104, i8 101, i8 111, i8 114, i8 121, i8 0]

@string_literal_149 = global [5 x i8] [i8 77, i8 97, i8 116, i8 104, i8 0]

@string_literal_150 = global [12 x i8] [i8 109, i8 97, i8 116, i8 104, i8 101, i8 109, i8 97, i8 116, i8 105, i8 99, i8 115, i8 0]

@string_literal_151 = global [6 x i8] [i8 77, i8 97, i8 116, i8 104, i8 115, i8 0]

@string_literal_152 = global [8 x i8] [i8 65, i8 108, i8 103, i8 101, i8 98, i8 114, i8 97, i8 0]

@string_literal_153 = global [6 x i8] [i8 71, i8 114, i8 97, i8 112, i8 104, i8 0]

@string_literal_154 = global [7 x i8] [i8 71, i8 114, i8 97, i8 112, i8 104, i8 115, i8 0]

@string_literal_155 = global [9 x i8] [i8 71, i8 101, i8 111, i8 109, i8 101, i8 116, i8 114, i8 121, i8 0]

@string_literal_156 = global [18 x i8] [i8 84, i8 114, i8 111, i8 112, i8 105, i8 99, i8 97, i8 108, i8 32, i8 71, i8 101, i8 111, i8 109, i8 101, i8 116, i8 114, i8 121, i8 0]

@string_literal_157 = global [15 x i8] [i8 80, i8 97, i8 114, i8 115, i8 101, i8 114, i8 32, i8 66, i8 117, i8 105, i8 108, i8 100, i8 101, i8 114, i8 0]

@string_literal_158 = global [19 x i8] [i8 80, i8 97, i8 114, i8 115, i8 101, i8 114, i8 32, i8 67, i8 111, i8 109, i8 98, i8 105, i8 110, i8 97, i8 116, i8 111, i8 114, i8 115, i8 0]

@string_literal_159 = global [7 x i8] [i8 80, i8 97, i8 114, i8 115, i8 101, i8 114, i8 0]

@string_literal_160 = global [18 x i8] [i8 80, i8 97, i8 114, i8 115, i8 101, i8 114, i8 67, i8 111, i8 109, i8 98, i8 105, i8 110, i8 97, i8 116, i8 111, i8 114, i8 115, i8 0]

@string_literal_161 = global [13 x i8] [i8 80, i8 97, i8 114, i8 115, i8 105, i8 110, i8 103, i8 32, i8 84, i8 101, i8 120, i8 116, i8 0]

@string_literal_162 = global [8 x i8] [i8 78, i8 101, i8 116, i8 119, i8 111, i8 114, i8 107, i8 0]

@string_literal_163 = global [13 x i8] [i8 68, i8 97, i8 116, i8 97, i8 32, i8 78, i8 101, i8 116, i8 119, i8 111, i8 114, i8 107, i8 0]

@string_literal_164 = global [13 x i8] [i8 78, i8 101, i8 116, i8 119, i8 111, i8 114, i8 107, i8 32, i8 65, i8 80, i8 73, i8 115, i8 0]

@string_literal_165 = global [16 x i8] [i8 78, i8 101, i8 116, i8 119, i8 111, i8 114, i8 107, i8 32, i8 67, i8 111, i8 110, i8 116, i8 114, i8 111, i8 108, i8 0]

@string_literal_166 = global [11 x i8] [i8 78, i8 101, i8 116, i8 119, i8 111, i8 114, i8 107, i8 65, i8 80, i8 73, i8 0]

@string_literal_167 = global [12 x i8] [i8 78, i8 101, i8 116, i8 119, i8 111, i8 114, i8 107, i8 65, i8 80, i8 73, i8 115, i8 0]

@string_literal_168 = global [11 x i8] [i8 78, i8 101, i8 116, i8 119, i8 111, i8 114, i8 107, i8 105, i8 110, i8 103, i8 0]

@string_literal_169 = global [4 x i8] [i8 87, i8 101, i8 98, i8 0]

@string_literal_170 = global [6 x i8] [i8 89, i8 101, i8 115, i8 111, i8 100, i8 0]

@string_literal_171 = global [11 x i8] [i8 74, i8 97, i8 118, i8 97, i8 115, i8 99, i8 114, i8 105, i8 112, i8 116, i8 0]

@string_literal_172 = global [8 x i8] [i8 79, i8 112, i8 101, i8 110, i8 65, i8 80, i8 73, i8 0]

@string_literal_173 = global [5 x i8] [i8 83, i8 110, i8 97, i8 112, i8 0]

@string_literal_174 = global [8 x i8] [i8 83, i8 101, i8 114, i8 118, i8 97, i8 110, i8 116, i8 0]

@string_literal_175 = global [12 x i8] [i8 83, i8 101, i8 114, i8 118, i8 97, i8 110, i8 116, i8 32, i8 87, i8 101, i8 98, i8 0]

@string_literal_176 = global [16 x i8] [i8 87, i8 101, i8 98, i8 32, i8 100, i8 101, i8 118, i8 101, i8 108, i8 111, i8 112, i8 109, i8 101, i8 110, i8 116, i8 0]

@string_literal_177 = global [10 x i8] [i8 72, i8 97, i8 112, i8 112, i8 115, i8 116, i8 97, i8 99, i8 107, i8 0]

@string_literal_178 = global [13 x i8] [i8 83, i8 101, i8 109, i8 97, i8 110, i8 116, i8 105, i8 99, i8 32, i8 87, i8 101, i8 98, i8 0]

@string_literal_179 = global [7 x i8] [i8 79, i8 112, i8 116, i8 105, i8 99, i8 115, i8 0]

@string_literal_180 = global [5 x i8] [i8 76, i8 101, i8 110, i8 115, i8 0]

@string_literal_181 = global [8 x i8] [i8 67, i8 111, i8 110, i8 100, i8 117, i8 105, i8 116, i8 0]

@string_literal_182 = global [9 x i8] [i8 83, i8 116, i8 114, i8 101, i8 97, i8 109, i8 108, i8 121, i8 0]

@string_literal_183 = global [6 x i8] [i8 80, i8 105, i8 112, i8 101, i8 115, i8 0]

@string_literal_184 = global [6 x i8] [i8 77, i8 111, i8 110, i8 97, i8 100, i8 0]

@string_literal_185 = global [8 x i8] [i8 77, i8 111, i8 110, i8 97, i8 100, i8 73, i8 79, i8 0]

@string_literal_186 = global [13 x i8] [i8 84, i8 114, i8 97, i8 110, i8 115, i8 102, i8 111, i8 114, i8 109, i8 101, i8 114, i8 115, i8 0]

@string_literal_187 = global [19 x i8] [i8 77, i8 111, i8 110, i8 97, i8 100, i8 32, i8 84, i8 114, i8 97, i8 110, i8 115, i8 102, i8 111, i8 114, i8 109, i8 101, i8 114, i8 115, i8 0]

@string_literal_188 = global [4 x i8] [i8 77, i8 116, i8 108, i8 0]

@string_literal_189 = global [16 x i8] [i8 85, i8 115, i8 101, i8 114, i8 32, i8 105, i8 110, i8 116, i8 101, i8 114, i8 102, i8 97, i8 99, i8 101, i8 115, i8 0]

@string_literal_190 = global [15 x i8] [i8 85, i8 115, i8 101, i8 114, i8 32, i8 105, i8 110, i8 116, i8 101, i8 114, i8 102, i8 97, i8 99, i8 101, i8 0]

@string_literal_191 = global [14 x i8] [i8 85, i8 115, i8 101, i8 114, i8 73, i8 110, i8 116, i8 101, i8 114, i8 102, i8 97, i8 99, i8 101, i8 0]

@string_literal_192 = global [3 x i8] [i8 85, i8 73, i8 0]

@string_literal_193 = global [16 x i8] [i8 85, i8 115, i8 101, i8 114, i8 32, i8 73, i8 110, i8 116, i8 101, i8 114, i8 102, i8 97, i8 99, i8 101, i8 115, i8 0]

@string_literal_194 = global [16 x i8] [i8 67, i8 111, i8 100, i8 101, i8 32, i8 71, i8 101, i8 110, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_195 = global [16 x i8] [i8 70, i8 111, i8 114, i8 101, i8 105, i8 103, i8 110, i8 32, i8 98, i8 105, i8 110, i8 100, i8 105, i8 110, i8 103, i8 0]

@string_literal_196 = global [4 x i8] [i8 69, i8 108, i8 109, i8 0]

@string_literal_197 = global [11 x i8] [i8 84, i8 121, i8 112, i8 101, i8 83, i8 99, i8 114, i8 105, i8 112, i8 116, i8 0]

@string_literal_198 = global [5 x i8] [i8 74, i8 97, i8 118, i8 97, i8 0]

@string_literal_199 = global [4 x i8] [i8 74, i8 86, i8 77, i8 0]

@string_literal_200 = global [4 x i8] [i8 74, i8 118, i8 109, i8 0]

@string_literal_201 = global [7 x i8] [i8 69, i8 114, i8 108, i8 97, i8 110, i8 103, i8 0]

@string_literal_202 = global [4 x i8] [i8 80, i8 72, i8 80, i8 0]

@string_literal_203 = global [8 x i8] [i8 70, i8 111, i8 114, i8 101, i8 105, i8 103, i8 110, i8 0]

@string_literal_204 = global [6 x i8] [i8 84, i8 121, i8 112, i8 101, i8 115, i8 0]

@string_literal_205 = global [9 x i8] [i8 86, i8 97, i8 108, i8 105, i8 100, i8 105, i8 116, i8 121, i8 0]

@string_literal_206 = global [11 x i8] [i8 81, i8 117, i8 105, i8 99, i8 107, i8 67, i8 104, i8 101, i8 99, i8 107, i8 0]

@string_literal_207 = global [5 x i8] [i8 84, i8 101, i8 115, i8 116, i8 0]

@string_literal_208 = global [6 x i8] [i8 83, i8 111, i8 117, i8 110, i8 100, i8 0]

@string_literal_209 = global [30 x i8] [i8 65, i8 108, i8 103, i8 111, i8 114, i8 105, i8 116, i8 104, i8 109, i8 105, i8 99, i8 32, i8 77, i8 117, i8 115, i8 105, i8 99, i8 32, i8 67, i8 111, i8 109, i8 112, i8 111, i8 115, i8 105, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_210 = global [27 x i8] [i8 65, i8 117, i8 116, i8 111, i8 109, i8 97, i8 116, i8 105, i8 99, i8 32, i8 77, i8 117, i8 115, i8 105, i8 99, i8 32, i8 71, i8 101, i8 110, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_211 = global [6 x i8] [i8 77, i8 117, i8 115, i8 105, i8 99, i8 0]

@string_literal_212 = global [4 x i8] [i8 90, i8 105, i8 112, i8 0]

@string_literal_213 = global [12 x i8] [i8 67, i8 111, i8 109, i8 112, i8 114, i8 101, i8 115, i8 115, i8 105, i8 111, i8 110, i8 0]

@string_literal_214 = global [5 x i8] [i8 90, i8 76, i8 105, i8 98, i8 0]

@string_literal_215 = global [4 x i8] [i8 84, i8 97, i8 114, i8 0]

@string_literal_216 = global [6 x i8] [i8 67, i8 108, i8 111, i8 117, i8 100, i8 0]

@string_literal_217 = global [7 x i8] [i8 71, i8 111, i8 111, i8 103, i8 108, i8 101, i8 0]

@string_literal_218 = global [4 x i8] [i8 65, i8 87, i8 83, i8 0]

@string_literal_219 = global [23 x i8] [i8 67, i8 111, i8 109, i8 112, i8 105, i8 108, i8 101, i8 114, i8 115, i8 47, i8 73, i8 110, i8 116, i8 101, i8 114, i8 112, i8 114, i8 101, i8 116, i8 101, i8 114, i8 115, i8 0]

@string_literal_220 = global [13 x i8] [i8 73, i8 110, i8 116, i8 101, i8 114, i8 112, i8 114, i8 101, i8 116, i8 101, i8 114, i8 115, i8 0]

@string_literal_221 = global [9 x i8] [i8 67, i8 111, i8 109, i8 112, i8 105, i8 108, i8 101, i8 114, i8 0]

@string_literal_222 = global [4 x i8] [i8 68, i8 83, i8 76, i8 0]

@string_literal_223 = global [9 x i8] [i8 68, i8 97, i8 116, i8 97, i8 98, i8 97, i8 115, i8 101, i8 0]

@string_literal_224 = global [11 x i8] [i8 80, i8 111, i8 115, i8 116, i8 103, i8 114, i8 101, i8 83, i8 81, i8 76, i8 0]

@string_literal_225 = global [4 x i8] [i8 78, i8 76, i8 80, i8 0]

@string_literal_226 = global [9 x i8] [i8 83, i8 116, i8 101, i8 109, i8 109, i8 105, i8 110, i8 103, i8 0]

@string_literal_227 = global [11 x i8] [i8 67, i8 111, i8 110, i8 116, i8 97, i8 105, i8 110, i8 101, i8 114, i8 115, i8 0]

@string_literal_228 = global [5 x i8] [i8 71, i8 97, i8 109, i8 101, i8 0]

@string_literal_229 = global [12 x i8] [i8 71, i8 97, i8 109, i8 101, i8 32, i8 69, i8 110, i8 103, i8 105, i8 110, i8 101, i8 0]

@string_literal_230 = global [11 x i8] [i8 67, i8 111, i8 110, i8 99, i8 117, i8 114, i8 114, i8 101, i8 110, i8 116, i8 0]

@string_literal_231 = global [9 x i8] [i8 80, i8 97, i8 114, i8 97, i8 108, i8 108, i8 101, i8 108, i8 0]

@string_literal_232 = global [22 x i8] [i8 68, i8 105, i8 115, i8 116, i8 114, i8 105, i8 98, i8 117, i8 116, i8 101, i8 100, i8 32, i8 67, i8 111, i8 109, i8 112, i8 117, i8 116, i8 105, i8 110, i8 103, i8 0]

@string_literal_233 = global [11 x i8] [i8 70, i8 105, i8 108, i8 101, i8 115, i8 121, i8 115, i8 116, i8 101, i8 109, i8 0]

@string_literal_234 = global [7 x i8] [i8 83, i8 121, i8 115, i8 116, i8 101, i8 109, i8 0]

@string_literal_235 = global [7 x i8] [i8 83, i8 89, i8 115, i8 116, i8 101, i8 109, i8 0]

@string_literal_236 = global [9 x i8] [i8 69, i8 109, i8 98, i8 101, i8 100, i8 100, i8 101, i8 100, i8 0]

@string_literal_237 = global [13 x i8] [i8 68, i8 105, i8 115, i8 116, i8 114, i8 105, i8 98, i8 117, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_238 = global [6 x i8] [i8 84, i8 114, i8 97, i8 99, i8 101, i8 0]

@string_literal_239 = global [6 x i8] [i8 68, i8 101, i8 98, i8 117, i8 103, i8 0]

@string_literal_240 = global [10 x i8] [i8 68, i8 101, i8 98, i8 117, i8 103, i8 103, i8 105, i8 110, i8 103, i8 0]

@string_literal_241 = global [14 x i8] [i8 79, i8 112, i8 101, i8 110, i8 84, i8 101, i8 108, i8 101, i8 109, i8 101, i8 116, i8 114, i8 121, i8 0]

@string_literal_242 = global [8 x i8] [i8 77, i8 101, i8 116, i8 114, i8 105, i8 99, i8 115, i8 0]

@string_literal_243 = global [6 x i8] [i8 82, i8 101, i8 103, i8 101, i8 120, i8 0]

define external ccc %program* @eclair_program_init() "wasm-export-name"="eclair_program_init" {
start:
  %0 = call ccc i8* @malloc(i32 1640)
  %1 = bitcast i8* %0 to %program*
  %2 = getelementptr %program, %program* %1, i32 0, i32 0
  call ccc void @eclair_symbol_table_init(%symbol_table* %2)
  %3 = getelementptr %program, %program* %1, i32 0, i32 1
  call ccc void @eclair_btree_init_empty_0(%btree_t_0* %3)
  %4 = getelementptr %program, %program* %1, i32 0, i32 2
  call ccc void @eclair_btree_init_empty_1(%btree_t_1* %4)
  %5 = getelementptr %program, %program* %1, i32 0, i32 3
  call ccc void @eclair_btree_init_empty_2(%btree_t_2* %5)
  %6 = getelementptr %program, %program* %1, i32 0, i32 4
  call ccc void @eclair_btree_init_empty_2(%btree_t_2* %6)
  %7 = getelementptr %program, %program* %1, i32 0, i32 5
  call ccc void @eclair_btree_init_empty_2(%btree_t_2* %7)
  %8 = getelementptr %program, %program* %1, i32 0, i32 0
  %9 = getelementptr inbounds [22 x i8], [22 x i8]* @string_literal_0, i32 0, i32 0
  %10 = zext i32 21 to i64
  %11 = call ccc i8* @malloc(i32 21)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %11, i8* %9, i64 %10, i1 0)
  %12 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %12, i32 21, i8* %11)
  %13 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %8, %symbol_t* %12)
  %14 = getelementptr %program, %program* %1, i32 0, i32 0
  %15 = getelementptr inbounds [28 x i8], [28 x i8]* @string_literal_1, i32 0, i32 0
  %16 = zext i32 27 to i64
  %17 = call ccc i8* @malloc(i32 27)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %17, i8* %15, i64 %16, i1 0)
  %18 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %18, i32 27, i8* %17)
  %19 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %14, %symbol_t* %18)
  %20 = getelementptr %program, %program* %1, i32 0, i32 0
  %21 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_2, i32 0, i32 0
  %22 = zext i32 15 to i64
  %23 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %23, i8* %21, i64 %22, i1 0)
  %24 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %24, i32 15, i8* %23)
  %25 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %20, %symbol_t* %24)
  %26 = getelementptr %program, %program* %1, i32 0, i32 0
  %27 = getelementptr inbounds [15 x i8], [15 x i8]* @string_literal_3, i32 0, i32 0
  %28 = zext i32 14 to i64
  %29 = call ccc i8* @malloc(i32 14)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %29, i8* %27, i64 %28, i1 0)
  %30 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %30, i32 14, i8* %29)
  %31 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %26, %symbol_t* %30)
  %32 = getelementptr %program, %program* %1, i32 0, i32 0
  %33 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_4, i32 0, i32 0
  %34 = zext i32 10 to i64
  %35 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %35, i8* %33, i64 %34, i1 0)
  %36 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %36, i32 10, i8* %35)
  %37 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %32, %symbol_t* %36)
  %38 = getelementptr %program, %program* %1, i32 0, i32 0
  %39 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_5, i32 0, i32 0
  %40 = zext i32 10 to i64
  %41 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %41, i8* %39, i64 %40, i1 0)
  %42 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %42, i32 10, i8* %41)
  %43 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %38, %symbol_t* %42)
  %44 = getelementptr %program, %program* %1, i32 0, i32 0
  %45 = getelementptr inbounds [59 x i8], [59 x i8]* @string_literal_6, i32 0, i32 0
  %46 = zext i32 58 to i64
  %47 = call ccc i8* @malloc(i32 58)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %47, i8* %45, i64 %46, i1 0)
  %48 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %48, i32 58, i8* %47)
  %49 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %44, %symbol_t* %48)
  %50 = getelementptr %program, %program* %1, i32 0, i32 0
  %51 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_7, i32 0, i32 0
  %52 = zext i32 5 to i64
  %53 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %53, i8* %51, i64 %52, i1 0)
  %54 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %54, i32 5, i8* %53)
  %55 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %50, %symbol_t* %54)
  %56 = getelementptr %program, %program* %1, i32 0, i32 0
  %57 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_8, i32 0, i32 0
  %58 = zext i32 5 to i64
  %59 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %59, i8* %57, i64 %58, i1 0)
  %60 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %60, i32 5, i8* %59)
  %61 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %56, %symbol_t* %60)
  %62 = getelementptr %program, %program* %1, i32 0, i32 0
  %63 = getelementptr inbounds [35 x i8], [35 x i8]* @string_literal_9, i32 0, i32 0
  %64 = zext i32 34 to i64
  %65 = call ccc i8* @malloc(i32 34)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %65, i8* %63, i64 %64, i1 0)
  %66 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %66, i32 34, i8* %65)
  %67 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %62, %symbol_t* %66)
  %68 = getelementptr %program, %program* %1, i32 0, i32 0
  %69 = getelementptr inbounds [15 x i8], [15 x i8]* @string_literal_10, i32 0, i32 0
  %70 = zext i32 14 to i64
  %71 = call ccc i8* @malloc(i32 14)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %71, i8* %69, i64 %70, i1 0)
  %72 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %72, i32 14, i8* %71)
  %73 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %68, %symbol_t* %72)
  %74 = getelementptr %program, %program* %1, i32 0, i32 0
  %75 = getelementptr inbounds [15 x i8], [15 x i8]* @string_literal_11, i32 0, i32 0
  %76 = zext i32 14 to i64
  %77 = call ccc i8* @malloc(i32 14)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %77, i8* %75, i64 %76, i1 0)
  %78 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %78, i32 14, i8* %77)
  %79 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %74, %symbol_t* %78)
  %80 = getelementptr %program, %program* %1, i32 0, i32 0
  %81 = getelementptr inbounds [57 x i8], [57 x i8]* @string_literal_12, i32 0, i32 0
  %82 = zext i32 56 to i64
  %83 = call ccc i8* @malloc(i32 56)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %83, i8* %81, i64 %82, i1 0)
  %84 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %84, i32 56, i8* %83)
  %85 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %80, %symbol_t* %84)
  %86 = getelementptr %program, %program* %1, i32 0, i32 0
  %87 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_13, i32 0, i32 0
  %88 = zext i32 5 to i64
  %89 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %89, i8* %87, i64 %88, i1 0)
  %90 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %90, i32 5, i8* %89)
  %91 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %86, %symbol_t* %90)
  %92 = getelementptr %program, %program* %1, i32 0, i32 0
  %93 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_14, i32 0, i32 0
  %94 = zext i32 15 to i64
  %95 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %95, i8* %93, i64 %94, i1 0)
  %96 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %96, i32 15, i8* %95)
  %97 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %92, %symbol_t* %96)
  %98 = getelementptr %program, %program* %1, i32 0, i32 0
  %99 = getelementptr inbounds [38 x i8], [38 x i8]* @string_literal_15, i32 0, i32 0
  %100 = zext i32 37 to i64
  %101 = call ccc i8* @malloc(i32 37)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %101, i8* %99, i64 %100, i1 0)
  %102 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %102, i32 37, i8* %101)
  %103 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %98, %symbol_t* %102)
  %104 = getelementptr %program, %program* %1, i32 0, i32 0
  %105 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_16, i32 0, i32 0
  %106 = zext i32 12 to i64
  %107 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %107, i8* %105, i64 %106, i1 0)
  %108 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %108, i32 12, i8* %107)
  %109 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %104, %symbol_t* %108)
  %110 = getelementptr %program, %program* %1, i32 0, i32 0
  %111 = getelementptr inbounds [18 x i8], [18 x i8]* @string_literal_17, i32 0, i32 0
  %112 = zext i32 17 to i64
  %113 = call ccc i8* @malloc(i32 17)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %113, i8* %111, i64 %112, i1 0)
  %114 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %114, i32 17, i8* %113)
  %115 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %110, %symbol_t* %114)
  %116 = getelementptr %program, %program* %1, i32 0, i32 0
  %117 = getelementptr inbounds [46 x i8], [46 x i8]* @string_literal_18, i32 0, i32 0
  %118 = zext i32 45 to i64
  %119 = call ccc i8* @malloc(i32 45)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %119, i8* %117, i64 %118, i1 0)
  %120 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %120, i32 45, i8* %119)
  %121 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %116, %symbol_t* %120)
  %122 = getelementptr %program, %program* %1, i32 0, i32 0
  %123 = getelementptr inbounds [23 x i8], [23 x i8]* @string_literal_19, i32 0, i32 0
  %124 = zext i32 22 to i64
  %125 = call ccc i8* @malloc(i32 22)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %125, i8* %123, i64 %124, i1 0)
  %126 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %126, i32 22, i8* %125)
  %127 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %122, %symbol_t* %126)
  %128 = getelementptr %program, %program* %1, i32 0, i32 0
  %129 = getelementptr inbounds [27 x i8], [27 x i8]* @string_literal_20, i32 0, i32 0
  %130 = zext i32 26 to i64
  %131 = call ccc i8* @malloc(i32 26)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %131, i8* %129, i64 %130, i1 0)
  %132 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %132, i32 26, i8* %131)
  %133 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %128, %symbol_t* %132)
  %134 = getelementptr %program, %program* %1, i32 0, i32 0
  %135 = getelementptr inbounds [45 x i8], [45 x i8]* @string_literal_21, i32 0, i32 0
  %136 = zext i32 44 to i64
  %137 = call ccc i8* @malloc(i32 44)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %137, i8* %135, i64 %136, i1 0)
  %138 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %138, i32 44, i8* %137)
  %139 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %134, %symbol_t* %138)
  %140 = getelementptr %program, %program* %1, i32 0, i32 0
  %141 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_22, i32 0, i32 0
  %142 = zext i32 11 to i64
  %143 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %143, i8* %141, i64 %142, i1 0)
  %144 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %144, i32 11, i8* %143)
  %145 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %140, %symbol_t* %144)
  %146 = getelementptr %program, %program* %1, i32 0, i32 0
  %147 = getelementptr inbounds [17 x i8], [17 x i8]* @string_literal_23, i32 0, i32 0
  %148 = zext i32 16 to i64
  %149 = call ccc i8* @malloc(i32 16)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %149, i8* %147, i64 %148, i1 0)
  %150 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %150, i32 16, i8* %149)
  %151 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %146, %symbol_t* %150)
  %152 = getelementptr %program, %program* %1, i32 0, i32 0
  %153 = getelementptr inbounds [28 x i8], [28 x i8]* @string_literal_24, i32 0, i32 0
  %154 = zext i32 27 to i64
  %155 = call ccc i8* @malloc(i32 27)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %155, i8* %153, i64 %154, i1 0)
  %156 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %156, i32 27, i8* %155)
  %157 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %152, %symbol_t* %156)
  %158 = getelementptr %program, %program* %1, i32 0, i32 0
  %159 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_25, i32 0, i32 0
  %160 = zext i32 11 to i64
  %161 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %161, i8* %159, i64 %160, i1 0)
  %162 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %162, i32 11, i8* %161)
  %163 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %158, %symbol_t* %162)
  %164 = getelementptr %program, %program* %1, i32 0, i32 0
  %165 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_26, i32 0, i32 0
  %166 = zext i32 11 to i64
  %167 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %167, i8* %165, i64 %166, i1 0)
  %168 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %168, i32 11, i8* %167)
  %169 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %164, %symbol_t* %168)
  %170 = getelementptr %program, %program* %1, i32 0, i32 0
  %171 = getelementptr inbounds [44 x i8], [44 x i8]* @string_literal_27, i32 0, i32 0
  %172 = zext i32 43 to i64
  %173 = call ccc i8* @malloc(i32 43)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %173, i8* %171, i64 %172, i1 0)
  %174 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %174, i32 43, i8* %173)
  %175 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %170, %symbol_t* %174)
  %176 = getelementptr %program, %program* %1, i32 0, i32 0
  %177 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_28, i32 0, i32 0
  %178 = zext i32 12 to i64
  %179 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %179, i8* %177, i64 %178, i1 0)
  %180 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %180, i32 12, i8* %179)
  %181 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %176, %symbol_t* %180)
  %182 = getelementptr %program, %program* %1, i32 0, i32 0
  %183 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_29, i32 0, i32 0
  %184 = zext i32 12 to i64
  %185 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %185, i8* %183, i64 %184, i1 0)
  %186 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %186, i32 12, i8* %185)
  %187 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %182, %symbol_t* %186)
  %188 = getelementptr %program, %program* %1, i32 0, i32 0
  %189 = getelementptr inbounds [43 x i8], [43 x i8]* @string_literal_30, i32 0, i32 0
  %190 = zext i32 42 to i64
  %191 = call ccc i8* @malloc(i32 42)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %191, i8* %189, i64 %190, i1 0)
  %192 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %192, i32 42, i8* %191)
  %193 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %188, %symbol_t* %192)
  %194 = getelementptr %program, %program* %1, i32 0, i32 0
  %195 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_31, i32 0, i32 0
  %196 = zext i32 15 to i64
  %197 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %197, i8* %195, i64 %196, i1 0)
  %198 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %198, i32 15, i8* %197)
  %199 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %194, %symbol_t* %198)
  %200 = getelementptr %program, %program* %1, i32 0, i32 0
  %201 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_32, i32 0, i32 0
  %202 = zext i32 15 to i64
  %203 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %203, i8* %201, i64 %202, i1 0)
  %204 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %204, i32 15, i8* %203)
  %205 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %200, %symbol_t* %204)
  %206 = getelementptr %program, %program* %1, i32 0, i32 0
  %207 = getelementptr inbounds [54 x i8], [54 x i8]* @string_literal_33, i32 0, i32 0
  %208 = zext i32 53 to i64
  %209 = call ccc i8* @malloc(i32 53)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %209, i8* %207, i64 %208, i1 0)
  %210 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %210, i32 53, i8* %209)
  %211 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %206, %symbol_t* %210)
  %212 = getelementptr %program, %program* %1, i32 0, i32 0
  %213 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_34, i32 0, i32 0
  %214 = zext i32 9 to i64
  %215 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %215, i8* %213, i64 %214, i1 0)
  %216 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %216, i32 9, i8* %215)
  %217 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %212, %symbol_t* %216)
  %218 = getelementptr %program, %program* %1, i32 0, i32 0
  %219 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_35, i32 0, i32 0
  %220 = zext i32 9 to i64
  %221 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %221, i8* %219, i64 %220, i1 0)
  %222 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %222, i32 9, i8* %221)
  %223 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %218, %symbol_t* %222)
  %224 = getelementptr %program, %program* %1, i32 0, i32 0
  %225 = getelementptr inbounds [32 x i8], [32 x i8]* @string_literal_36, i32 0, i32 0
  %226 = zext i32 31 to i64
  %227 = call ccc i8* @malloc(i32 31)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %227, i8* %225, i64 %226, i1 0)
  %228 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %228, i32 31, i8* %227)
  %229 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %224, %symbol_t* %228)
  %230 = getelementptr %program, %program* %1, i32 0, i32 0
  %231 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_37, i32 0, i32 0
  %232 = zext i32 11 to i64
  %233 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %233, i8* %231, i64 %232, i1 0)
  %234 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %234, i32 11, i8* %233)
  %235 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %230, %symbol_t* %234)
  %236 = getelementptr %program, %program* %1, i32 0, i32 0
  %237 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_38, i32 0, i32 0
  %238 = zext i32 11 to i64
  %239 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %239, i8* %237, i64 %238, i1 0)
  %240 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %240, i32 11, i8* %239)
  %241 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %236, %symbol_t* %240)
  %242 = getelementptr %program, %program* %1, i32 0, i32 0
  %243 = getelementptr inbounds [54 x i8], [54 x i8]* @string_literal_39, i32 0, i32 0
  %244 = zext i32 53 to i64
  %245 = call ccc i8* @malloc(i32 53)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %245, i8* %243, i64 %244, i1 0)
  %246 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %246, i32 53, i8* %245)
  %247 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %242, %symbol_t* %246)
  %248 = getelementptr %program, %program* %1, i32 0, i32 0
  %249 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_40, i32 0, i32 0
  %250 = zext i32 11 to i64
  %251 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %251, i8* %249, i64 %250, i1 0)
  %252 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %252, i32 11, i8* %251)
  %253 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %248, %symbol_t* %252)
  %254 = getelementptr %program, %program* %1, i32 0, i32 0
  %255 = getelementptr inbounds [34 x i8], [34 x i8]* @string_literal_41, i32 0, i32 0
  %256 = zext i32 33 to i64
  %257 = call ccc i8* @malloc(i32 33)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %257, i8* %255, i64 %256, i1 0)
  %258 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %258, i32 33, i8* %257)
  %259 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %254, %symbol_t* %258)
  %260 = getelementptr %program, %program* %1, i32 0, i32 0
  %261 = getelementptr inbounds [55 x i8], [55 x i8]* @string_literal_42, i32 0, i32 0
  %262 = zext i32 54 to i64
  %263 = call ccc i8* @malloc(i32 54)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %263, i8* %261, i64 %262, i1 0)
  %264 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %264, i32 54, i8* %263)
  %265 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %260, %symbol_t* %264)
  %266 = getelementptr %program, %program* %1, i32 0, i32 0
  %267 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_43, i32 0, i32 0
  %268 = zext i32 12 to i64
  %269 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %269, i8* %267, i64 %268, i1 0)
  %270 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %270, i32 12, i8* %269)
  %271 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %266, %symbol_t* %270)
  %272 = getelementptr %program, %program* %1, i32 0, i32 0
  %273 = getelementptr inbounds [21 x i8], [21 x i8]* @string_literal_44, i32 0, i32 0
  %274 = zext i32 20 to i64
  %275 = call ccc i8* @malloc(i32 20)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %275, i8* %273, i64 %274, i1 0)
  %276 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %276, i32 20, i8* %275)
  %277 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %272, %symbol_t* %276)
  %278 = getelementptr %program, %program* %1, i32 0, i32 0
  %279 = getelementptr inbounds [57 x i8], [57 x i8]* @string_literal_45, i32 0, i32 0
  %280 = zext i32 56 to i64
  %281 = call ccc i8* @malloc(i32 56)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %281, i8* %279, i64 %280, i1 0)
  %282 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %282, i32 56, i8* %281)
  %283 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %278, %symbol_t* %282)
  %284 = getelementptr %program, %program* %1, i32 0, i32 0
  %285 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_46, i32 0, i32 0
  %286 = zext i32 3 to i64
  %287 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %287, i8* %285, i64 %286, i1 0)
  %288 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %288, i32 3, i8* %287)
  %289 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %284, %symbol_t* %288)
  %290 = getelementptr %program, %program* %1, i32 0, i32 0
  %291 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_47, i32 0, i32 0
  %292 = zext i32 3 to i64
  %293 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %293, i8* %291, i64 %292, i1 0)
  %294 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %294, i32 3, i8* %293)
  %295 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %290, %symbol_t* %294)
  %296 = getelementptr %program, %program* %1, i32 0, i32 0
  %297 = getelementptr inbounds [53 x i8], [53 x i8]* @string_literal_48, i32 0, i32 0
  %298 = zext i32 52 to i64
  %299 = call ccc i8* @malloc(i32 52)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %299, i8* %297, i64 %298, i1 0)
  %300 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %300, i32 52, i8* %299)
  %301 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %296, %symbol_t* %300)
  %302 = getelementptr %program, %program* %1, i32 0, i32 0
  %303 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_49, i32 0, i32 0
  %304 = zext i32 3 to i64
  %305 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %305, i8* %303, i64 %304, i1 0)
  %306 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %306, i32 3, i8* %305)
  %307 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %302, %symbol_t* %306)
  %308 = getelementptr %program, %program* %1, i32 0, i32 0
  %309 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_50, i32 0, i32 0
  %310 = zext i32 3 to i64
  %311 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %311, i8* %309, i64 %310, i1 0)
  %312 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %312, i32 3, i8* %311)
  %313 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %308, %symbol_t* %312)
  %314 = getelementptr %program, %program* %1, i32 0, i32 0
  %315 = getelementptr inbounds [32 x i8], [32 x i8]* @string_literal_51, i32 0, i32 0
  %316 = zext i32 31 to i64
  %317 = call ccc i8* @malloc(i32 31)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %317, i8* %315, i64 %316, i1 0)
  %318 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %318, i32 31, i8* %317)
  %319 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %314, %symbol_t* %318)
  %320 = getelementptr %program, %program* %1, i32 0, i32 0
  %321 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_52, i32 0, i32 0
  %322 = zext i32 8 to i64
  %323 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %323, i8* %321, i64 %322, i1 0)
  %324 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %324, i32 8, i8* %323)
  %325 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %320, %symbol_t* %324)
  %326 = getelementptr %program, %program* %1, i32 0, i32 0
  %327 = getelementptr inbounds [17 x i8], [17 x i8]* @string_literal_53, i32 0, i32 0
  %328 = zext i32 16 to i64
  %329 = call ccc i8* @malloc(i32 16)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %329, i8* %327, i64 %328, i1 0)
  %330 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %330, i32 16, i8* %329)
  %331 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %326, %symbol_t* %330)
  %332 = getelementptr %program, %program* %1, i32 0, i32 0
  %333 = getelementptr inbounds [36 x i8], [36 x i8]* @string_literal_54, i32 0, i32 0
  %334 = zext i32 35 to i64
  %335 = call ccc i8* @malloc(i32 35)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %335, i8* %333, i64 %334, i1 0)
  %336 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %336, i32 35, i8* %335)
  %337 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %332, %symbol_t* %336)
  %338 = getelementptr %program, %program* %1, i32 0, i32 0
  %339 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_55, i32 0, i32 0
  %340 = zext i32 8 to i64
  %341 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %341, i8* %339, i64 %340, i1 0)
  %342 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %342, i32 8, i8* %341)
  %343 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %338, %symbol_t* %342)
  %344 = getelementptr %program, %program* %1, i32 0, i32 0
  %345 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_56, i32 0, i32 0
  %346 = zext i32 8 to i64
  %347 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %347, i8* %345, i64 %346, i1 0)
  %348 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %348, i32 8, i8* %347)
  %349 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %344, %symbol_t* %348)
  %350 = getelementptr %program, %program* %1, i32 0, i32 0
  %351 = getelementptr inbounds [42 x i8], [42 x i8]* @string_literal_57, i32 0, i32 0
  %352 = zext i32 41 to i64
  %353 = call ccc i8* @malloc(i32 41)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %353, i8* %351, i64 %352, i1 0)
  %354 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %354, i32 41, i8* %353)
  %355 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %350, %symbol_t* %354)
  %356 = getelementptr %program, %program* %1, i32 0, i32 0
  %357 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_58, i32 0, i32 0
  %358 = zext i32 8 to i64
  %359 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %359, i8* %357, i64 %358, i1 0)
  %360 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %360, i32 8, i8* %359)
  %361 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %356, %symbol_t* %360)
  %362 = getelementptr %program, %program* %1, i32 0, i32 0
  %363 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_59, i32 0, i32 0
  %364 = zext i32 8 to i64
  %365 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %365, i8* %363, i64 %364, i1 0)
  %366 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %366, i32 8, i8* %365)
  %367 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %362, %symbol_t* %366)
  %368 = getelementptr %program, %program* %1, i32 0, i32 0
  %369 = getelementptr inbounds [40 x i8], [40 x i8]* @string_literal_60, i32 0, i32 0
  %370 = zext i32 39 to i64
  %371 = call ccc i8* @malloc(i32 39)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %371, i8* %369, i64 %370, i1 0)
  %372 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %372, i32 39, i8* %371)
  %373 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %368, %symbol_t* %372)
  %374 = getelementptr %program, %program* %1, i32 0, i32 0
  %375 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_61, i32 0, i32 0
  %376 = zext i32 3 to i64
  %377 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %377, i8* %375, i64 %376, i1 0)
  %378 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %378, i32 3, i8* %377)
  %379 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %374, %symbol_t* %378)
  %380 = getelementptr %program, %program* %1, i32 0, i32 0
  %381 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_62, i32 0, i32 0
  %382 = zext i32 3 to i64
  %383 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %383, i8* %381, i64 %382, i1 0)
  %384 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %384, i32 3, i8* %383)
  %385 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %380, %symbol_t* %384)
  %386 = getelementptr %program, %program* %1, i32 0, i32 0
  %387 = getelementptr inbounds [35 x i8], [35 x i8]* @string_literal_63, i32 0, i32 0
  %388 = zext i32 34 to i64
  %389 = call ccc i8* @malloc(i32 34)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %389, i8* %387, i64 %388, i1 0)
  %390 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %390, i32 34, i8* %389)
  %391 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %386, %symbol_t* %390)
  %392 = getelementptr %program, %program* %1, i32 0, i32 0
  %393 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_64, i32 0, i32 0
  %394 = zext i32 8 to i64
  %395 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %395, i8* %393, i64 %394, i1 0)
  %396 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %396, i32 8, i8* %395)
  %397 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %392, %symbol_t* %396)
  %398 = getelementptr %program, %program* %1, i32 0, i32 0
  %399 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_65, i32 0, i32 0
  %400 = zext i32 8 to i64
  %401 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %401, i8* %399, i64 %400, i1 0)
  %402 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %402, i32 8, i8* %401)
  %403 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %398, %symbol_t* %402)
  %404 = getelementptr %program, %program* %1, i32 0, i32 0
  %405 = getelementptr inbounds [53 x i8], [53 x i8]* @string_literal_66, i32 0, i32 0
  %406 = zext i32 52 to i64
  %407 = call ccc i8* @malloc(i32 52)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %407, i8* %405, i64 %406, i1 0)
  %408 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %408, i32 52, i8* %407)
  %409 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %404, %symbol_t* %408)
  %410 = getelementptr %program, %program* %1, i32 0, i32 0
  %411 = getelementptr inbounds [5 x i8], [5 x i8]* @string_literal_67, i32 0, i32 0
  %412 = zext i32 4 to i64
  %413 = call ccc i8* @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %413, i8* %411, i64 %412, i1 0)
  %414 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %414, i32 4, i8* %413)
  %415 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %410, %symbol_t* %414)
  %416 = getelementptr %program, %program* %1, i32 0, i32 0
  %417 = getelementptr inbounds [5 x i8], [5 x i8]* @string_literal_68, i32 0, i32 0
  %418 = zext i32 4 to i64
  %419 = call ccc i8* @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %419, i8* %417, i64 %418, i1 0)
  %420 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %420, i32 4, i8* %419)
  %421 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %416, %symbol_t* %420)
  %422 = getelementptr %program, %program* %1, i32 0, i32 0
  %423 = getelementptr inbounds [41 x i8], [41 x i8]* @string_literal_69, i32 0, i32 0
  %424 = zext i32 40 to i64
  %425 = call ccc i8* @malloc(i32 40)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %425, i8* %423, i64 %424, i1 0)
  %426 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %426, i32 40, i8* %425)
  %427 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %422, %symbol_t* %426)
  %428 = getelementptr %program, %program* %1, i32 0, i32 0
  %429 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_70, i32 0, i32 0
  %430 = zext i32 8 to i64
  %431 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %431, i8* %429, i64 %430, i1 0)
  %432 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %432, i32 8, i8* %431)
  %433 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %428, %symbol_t* %432)
  %434 = getelementptr %program, %program* %1, i32 0, i32 0
  %435 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_71, i32 0, i32 0
  %436 = zext i32 8 to i64
  %437 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %437, i8* %435, i64 %436, i1 0)
  %438 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %438, i32 8, i8* %437)
  %439 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %434, %symbol_t* %438)
  %440 = getelementptr %program, %program* %1, i32 0, i32 0
  %441 = getelementptr inbounds [58 x i8], [58 x i8]* @string_literal_72, i32 0, i32 0
  %442 = zext i32 57 to i64
  %443 = call ccc i8* @malloc(i32 57)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %443, i8* %441, i64 %442, i1 0)
  %444 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %444, i32 57, i8* %443)
  %445 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %440, %symbol_t* %444)
  %446 = getelementptr %program, %program* %1, i32 0, i32 0
  %447 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_73, i32 0, i32 0
  %448 = zext i32 6 to i64
  %449 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %449, i8* %447, i64 %448, i1 0)
  %450 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %450, i32 6, i8* %449)
  %451 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %446, %symbol_t* %450)
  %452 = getelementptr %program, %program* %1, i32 0, i32 0
  %453 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_74, i32 0, i32 0
  %454 = zext i32 6 to i64
  %455 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %455, i8* %453, i64 %454, i1 0)
  %456 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %456, i32 6, i8* %455)
  %457 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %452, %symbol_t* %456)
  %458 = getelementptr %program, %program* %1, i32 0, i32 0
  %459 = getelementptr inbounds [59 x i8], [59 x i8]* @string_literal_75, i32 0, i32 0
  %460 = zext i32 58 to i64
  %461 = call ccc i8* @malloc(i32 58)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %461, i8* %459, i64 %460, i1 0)
  %462 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %462, i32 58, i8* %461)
  %463 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %458, %symbol_t* %462)
  %464 = getelementptr %program, %program* %1, i32 0, i32 0
  %465 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_76, i32 0, i32 0
  %466 = zext i32 5 to i64
  %467 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %467, i8* %465, i64 %466, i1 0)
  %468 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %468, i32 5, i8* %467)
  %469 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %464, %symbol_t* %468)
  %470 = getelementptr %program, %program* %1, i32 0, i32 0
  %471 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_77, i32 0, i32 0
  %472 = zext i32 11 to i64
  %473 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %473, i8* %471, i64 %472, i1 0)
  %474 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %474, i32 11, i8* %473)
  %475 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %470, %symbol_t* %474)
  %476 = getelementptr %program, %program* %1, i32 0, i32 0
  %477 = getelementptr inbounds [36 x i8], [36 x i8]* @string_literal_78, i32 0, i32 0
  %478 = zext i32 35 to i64
  %479 = call ccc i8* @malloc(i32 35)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %479, i8* %477, i64 %478, i1 0)
  %480 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %480, i32 35, i8* %479)
  %481 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %476, %symbol_t* %480)
  %482 = getelementptr %program, %program* %1, i32 0, i32 0
  %483 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_79, i32 0, i32 0
  %484 = zext i32 6 to i64
  %485 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %485, i8* %483, i64 %484, i1 0)
  %486 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %486, i32 6, i8* %485)
  %487 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %482, %symbol_t* %486)
  %488 = getelementptr %program, %program* %1, i32 0, i32 0
  %489 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_80, i32 0, i32 0
  %490 = zext i32 6 to i64
  %491 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %491, i8* %489, i64 %490, i1 0)
  %492 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %492, i32 6, i8* %491)
  %493 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %488, %symbol_t* %492)
  %494 = getelementptr %program, %program* %1, i32 0, i32 0
  %495 = getelementptr inbounds [34 x i8], [34 x i8]* @string_literal_81, i32 0, i32 0
  %496 = zext i32 33 to i64
  %497 = call ccc i8* @malloc(i32 33)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %497, i8* %495, i64 %496, i1 0)
  %498 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %498, i32 33, i8* %497)
  %499 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %494, %symbol_t* %498)
  %500 = getelementptr %program, %program* %1, i32 0, i32 0
  %501 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_82, i32 0, i32 0
  %502 = zext i32 7 to i64
  %503 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %503, i8* %501, i64 %502, i1 0)
  %504 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %504, i32 7, i8* %503)
  %505 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %500, %symbol_t* %504)
  %506 = getelementptr %program, %program* %1, i32 0, i32 0
  %507 = getelementptr inbounds [20 x i8], [20 x i8]* @string_literal_83, i32 0, i32 0
  %508 = zext i32 19 to i64
  %509 = call ccc i8* @malloc(i32 19)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %509, i8* %507, i64 %508, i1 0)
  %510 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %510, i32 19, i8* %509)
  %511 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %506, %symbol_t* %510)
  %512 = getelementptr %program, %program* %1, i32 0, i32 0
  %513 = getelementptr inbounds [63 x i8], [63 x i8]* @string_literal_84, i32 0, i32 0
  %514 = zext i32 62 to i64
  %515 = call ccc i8* @malloc(i32 62)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %515, i8* %513, i64 %514, i1 0)
  %516 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %516, i32 62, i8* %515)
  %517 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %512, %symbol_t* %516)
  %518 = getelementptr %program, %program* %1, i32 0, i32 0
  %519 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_85, i32 0, i32 0
  %520 = zext i32 3 to i64
  %521 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %521, i8* %519, i64 %520, i1 0)
  %522 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %522, i32 3, i8* %521)
  %523 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %518, %symbol_t* %522)
  %524 = getelementptr %program, %program* %1, i32 0, i32 0
  %525 = getelementptr inbounds [28 x i8], [28 x i8]* @string_literal_86, i32 0, i32 0
  %526 = zext i32 27 to i64
  %527 = call ccc i8* @malloc(i32 27)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %527, i8* %525, i64 %526, i1 0)
  %528 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %528, i32 27, i8* %527)
  %529 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %524, %symbol_t* %528)
  %530 = getelementptr %program, %program* %1, i32 0, i32 0
  %531 = getelementptr inbounds [39 x i8], [39 x i8]* @string_literal_87, i32 0, i32 0
  %532 = zext i32 38 to i64
  %533 = call ccc i8* @malloc(i32 38)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %533, i8* %531, i64 %532, i1 0)
  %534 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %534, i32 38, i8* %533)
  %535 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %530, %symbol_t* %534)
  %536 = getelementptr %program, %program* %1, i32 0, i32 0
  %537 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_88, i32 0, i32 0
  %538 = zext i32 11 to i64
  %539 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %539, i8* %537, i64 %538, i1 0)
  %540 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %540, i32 11, i8* %539)
  %541 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %536, %symbol_t* %540)
  %542 = getelementptr %program, %program* %1, i32 0, i32 0
  %543 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_89, i32 0, i32 0
  %544 = zext i32 11 to i64
  %545 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %545, i8* %543, i64 %544, i1 0)
  %546 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %546, i32 11, i8* %545)
  %547 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %542, %symbol_t* %546)
  %548 = getelementptr %program, %program* %1, i32 0, i32 0
  %549 = getelementptr inbounds [21 x i8], [21 x i8]* @string_literal_90, i32 0, i32 0
  %550 = zext i32 20 to i64
  %551 = call ccc i8* @malloc(i32 20)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %551, i8* %549, i64 %550, i1 0)
  %552 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %552, i32 20, i8* %551)
  %553 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %548, %symbol_t* %552)
  %554 = getelementptr %program, %program* %1, i32 0, i32 0
  %555 = getelementptr inbounds [23 x i8], [23 x i8]* @string_literal_91, i32 0, i32 0
  %556 = zext i32 22 to i64
  %557 = call ccc i8* @malloc(i32 22)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %557, i8* %555, i64 %556, i1 0)
  %558 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %558, i32 22, i8* %557)
  %559 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %554, %symbol_t* %558)
  %560 = getelementptr %program, %program* %1, i32 0, i32 0
  %561 = getelementptr inbounds [23 x i8], [23 x i8]* @string_literal_92, i32 0, i32 0
  %562 = zext i32 22 to i64
  %563 = call ccc i8* @malloc(i32 22)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %563, i8* %561, i64 %562, i1 0)
  %564 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %564, i32 22, i8* %563)
  %565 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %560, %symbol_t* %564)
  %566 = getelementptr %program, %program* %1, i32 0, i32 0
  %567 = getelementptr inbounds [21 x i8], [21 x i8]* @string_literal_93, i32 0, i32 0
  %568 = zext i32 20 to i64
  %569 = call ccc i8* @malloc(i32 20)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %569, i8* %567, i64 %568, i1 0)
  %570 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %570, i32 20, i8* %569)
  %571 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %566, %symbol_t* %570)
  %572 = getelementptr %program, %program* %1, i32 0, i32 0
  %573 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_94, i32 0, i32 0
  %574 = zext i32 7 to i64
  %575 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %575, i8* %573, i64 %574, i1 0)
  %576 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %576, i32 7, i8* %575)
  %577 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %572, %symbol_t* %576)
  %578 = getelementptr %program, %program* %1, i32 0, i32 0
  %579 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_95, i32 0, i32 0
  %580 = zext i32 7 to i64
  %581 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %581, i8* %579, i64 %580, i1 0)
  %582 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %582, i32 7, i8* %581)
  %583 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %578, %symbol_t* %582)
  %584 = getelementptr %program, %program* %1, i32 0, i32 0
  %585 = getelementptr inbounds [35 x i8], [35 x i8]* @string_literal_96, i32 0, i32 0
  %586 = zext i32 34 to i64
  %587 = call ccc i8* @malloc(i32 34)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %587, i8* %585, i64 %586, i1 0)
  %588 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %588, i32 34, i8* %587)
  %589 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %584, %symbol_t* %588)
  %590 = getelementptr %program, %program* %1, i32 0, i32 0
  %591 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_97, i32 0, i32 0
  %592 = zext i32 7 to i64
  %593 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %593, i8* %591, i64 %592, i1 0)
  %594 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %594, i32 7, i8* %593)
  %595 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %590, %symbol_t* %594)
  %596 = getelementptr %program, %program* %1, i32 0, i32 0
  %597 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_98, i32 0, i32 0
  %598 = zext i32 7 to i64
  %599 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %599, i8* %597, i64 %598, i1 0)
  %600 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %600, i32 7, i8* %599)
  %601 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %596, %symbol_t* %600)
  %602 = getelementptr %program, %program* %1, i32 0, i32 0
  %603 = getelementptr inbounds [62 x i8], [62 x i8]* @string_literal_99, i32 0, i32 0
  %604 = zext i32 61 to i64
  %605 = call ccc i8* @malloc(i32 61)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %605, i8* %603, i64 %604, i1 0)
  %606 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %606, i32 61, i8* %605)
  %607 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %602, %symbol_t* %606)
  %608 = getelementptr %program, %program* %1, i32 0, i32 0
  %609 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_100, i32 0, i32 0
  %610 = zext i32 7 to i64
  %611 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %611, i8* %609, i64 %610, i1 0)
  %612 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %612, i32 7, i8* %611)
  %613 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %608, %symbol_t* %612)
  %614 = getelementptr %program, %program* %1, i32 0, i32 0
  %615 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_101, i32 0, i32 0
  %616 = zext i32 7 to i64
  %617 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %617, i8* %615, i64 %616, i1 0)
  %618 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %618, i32 7, i8* %617)
  %619 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %614, %symbol_t* %618)
  %620 = getelementptr %program, %program* %1, i32 0, i32 0
  %621 = getelementptr inbounds [60 x i8], [60 x i8]* @string_literal_102, i32 0, i32 0
  %622 = zext i32 59 to i64
  %623 = call ccc i8* @malloc(i32 59)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %623, i8* %621, i64 %622, i1 0)
  %624 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %624, i32 59, i8* %623)
  %625 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %620, %symbol_t* %624)
  %626 = getelementptr %program, %program* %1, i32 0, i32 0
  %627 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_103, i32 0, i32 0
  %628 = zext i32 7 to i64
  %629 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %629, i8* %627, i64 %628, i1 0)
  %630 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %630, i32 7, i8* %629)
  %631 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %626, %symbol_t* %630)
  %632 = getelementptr %program, %program* %1, i32 0, i32 0
  %633 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_104, i32 0, i32 0
  %634 = zext i32 7 to i64
  %635 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %635, i8* %633, i64 %634, i1 0)
  %636 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %636, i32 7, i8* %635)
  %637 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %632, %symbol_t* %636)
  %638 = getelementptr %program, %program* %1, i32 0, i32 0
  %639 = getelementptr inbounds [39 x i8], [39 x i8]* @string_literal_105, i32 0, i32 0
  %640 = zext i32 38 to i64
  %641 = call ccc i8* @malloc(i32 38)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %641, i8* %639, i64 %640, i1 0)
  %642 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %642, i32 38, i8* %641)
  %643 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %638, %symbol_t* %642)
  %644 = getelementptr %program, %program* %1, i32 0, i32 0
  %645 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_106, i32 0, i32 0
  %646 = zext i32 9 to i64
  %647 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %647, i8* %645, i64 %646, i1 0)
  %648 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %648, i32 9, i8* %647)
  %649 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %644, %symbol_t* %648)
  %650 = getelementptr %program, %program* %1, i32 0, i32 0
  %651 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_107, i32 0, i32 0
  %652 = zext i32 9 to i64
  %653 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %653, i8* %651, i64 %652, i1 0)
  %654 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %654, i32 9, i8* %653)
  %655 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %650, %symbol_t* %654)
  %656 = getelementptr %program, %program* %1, i32 0, i32 0
  %657 = getelementptr inbounds [39 x i8], [39 x i8]* @string_literal_108, i32 0, i32 0
  %658 = zext i32 38 to i64
  %659 = call ccc i8* @malloc(i32 38)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %659, i8* %657, i64 %658, i1 0)
  %660 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %660, i32 38, i8* %659)
  %661 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %656, %symbol_t* %660)
  %662 = getelementptr %program, %program* %1, i32 0, i32 0
  %663 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_109, i32 0, i32 0
  %664 = zext i32 9 to i64
  %665 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %665, i8* %663, i64 %664, i1 0)
  %666 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %666, i32 9, i8* %665)
  %667 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %662, %symbol_t* %666)
  %668 = getelementptr %program, %program* %1, i32 0, i32 0
  %669 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_110, i32 0, i32 0
  %670 = zext i32 9 to i64
  %671 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %671, i8* %669, i64 %670, i1 0)
  %672 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %672, i32 9, i8* %671)
  %673 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %668, %symbol_t* %672)
  %674 = getelementptr %program, %program* %1, i32 0, i32 0
  %675 = getelementptr inbounds [41 x i8], [41 x i8]* @string_literal_111, i32 0, i32 0
  %676 = zext i32 40 to i64
  %677 = call ccc i8* @malloc(i32 40)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %677, i8* %675, i64 %676, i1 0)
  %678 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %678, i32 40, i8* %677)
  %679 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %674, %symbol_t* %678)
  %680 = getelementptr %program, %program* %1, i32 0, i32 0
  %681 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_112, i32 0, i32 0
  %682 = zext i32 6 to i64
  %683 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %683, i8* %681, i64 %682, i1 0)
  %684 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %684, i32 6, i8* %683)
  %685 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %680, %symbol_t* %684)
  %686 = getelementptr %program, %program* %1, i32 0, i32 0
  %687 = getelementptr inbounds [20 x i8], [20 x i8]* @string_literal_113, i32 0, i32 0
  %688 = zext i32 19 to i64
  %689 = call ccc i8* @malloc(i32 19)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %689, i8* %687, i64 %688, i1 0)
  %690 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %690, i32 19, i8* %689)
  %691 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %686, %symbol_t* %690)
  %692 = getelementptr %program, %program* %1, i32 0, i32 0
  %693 = getelementptr inbounds [56 x i8], [56 x i8]* @string_literal_114, i32 0, i32 0
  %694 = zext i32 55 to i64
  %695 = call ccc i8* @malloc(i32 55)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %695, i8* %693, i64 %694, i1 0)
  %696 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %696, i32 55, i8* %695)
  %697 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %692, %symbol_t* %696)
  %698 = getelementptr %program, %program* %1, i32 0, i32 0
  %699 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_115, i32 0, i32 0
  %700 = zext i32 9 to i64
  %701 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %701, i8* %699, i64 %700, i1 0)
  %702 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %702, i32 9, i8* %701)
  %703 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %698, %symbol_t* %702)
  %704 = getelementptr %program, %program* %1, i32 0, i32 0
  %705 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_116, i32 0, i32 0
  %706 = zext i32 9 to i64
  %707 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %707, i8* %705, i64 %706, i1 0)
  %708 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %708, i32 9, i8* %707)
  %709 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %704, %symbol_t* %708)
  %710 = getelementptr %program, %program* %1, i32 0, i32 0
  %711 = getelementptr inbounds [22 x i8], [22 x i8]* @string_literal_117, i32 0, i32 0
  %712 = zext i32 21 to i64
  %713 = call ccc i8* @malloc(i32 21)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %713, i8* %711, i64 %712, i1 0)
  %714 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %714, i32 21, i8* %713)
  %715 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %710, %symbol_t* %714)
  %716 = getelementptr %program, %program* %1, i32 0, i32 0
  %717 = getelementptr inbounds [17 x i8], [17 x i8]* @string_literal_118, i32 0, i32 0
  %718 = zext i32 16 to i64
  %719 = call ccc i8* @malloc(i32 16)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %719, i8* %717, i64 %718, i1 0)
  %720 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %720, i32 16, i8* %719)
  %721 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %716, %symbol_t* %720)
  %722 = getelementptr %program, %program* %1, i32 0, i32 0
  %723 = getelementptr inbounds [17 x i8], [17 x i8]* @string_literal_119, i32 0, i32 0
  %724 = zext i32 16 to i64
  %725 = call ccc i8* @malloc(i32 16)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %725, i8* %723, i64 %724, i1 0)
  %726 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %726, i32 16, i8* %725)
  %727 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %722, %symbol_t* %726)
  %728 = getelementptr %program, %program* %1, i32 0, i32 0
  %729 = getelementptr inbounds [38 x i8], [38 x i8]* @string_literal_120, i32 0, i32 0
  %730 = zext i32 37 to i64
  %731 = call ccc i8* @malloc(i32 37)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %731, i8* %729, i64 %730, i1 0)
  %732 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %732, i32 37, i8* %731)
  %733 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %728, %symbol_t* %732)
  %734 = getelementptr %program, %program* %1, i32 0, i32 0
  %735 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_121, i32 0, i32 0
  %736 = zext i32 7 to i64
  %737 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %737, i8* %735, i64 %736, i1 0)
  %738 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %738, i32 7, i8* %737)
  %739 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %734, %symbol_t* %738)
  %740 = getelementptr %program, %program* %1, i32 0, i32 0
  %741 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_122, i32 0, i32 0
  %742 = zext i32 7 to i64
  %743 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %743, i8* %741, i64 %742, i1 0)
  %744 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %744, i32 7, i8* %743)
  %745 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %740, %symbol_t* %744)
  %746 = getelementptr %program, %program* %1, i32 0, i32 0
  %747 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_123, i32 0, i32 0
  %748 = zext i32 15 to i64
  %749 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %749, i8* %747, i64 %748, i1 0)
  %750 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %750, i32 15, i8* %749)
  %751 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %746, %symbol_t* %750)
  %752 = getelementptr %program, %program* %1, i32 0, i32 0
  %753 = getelementptr inbounds [5 x i8], [5 x i8]* @string_literal_124, i32 0, i32 0
  %754 = zext i32 4 to i64
  %755 = call ccc i8* @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %755, i8* %753, i64 %754, i1 0)
  %756 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %756, i32 4, i8* %755)
  %757 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %752, %symbol_t* %756)
  %758 = getelementptr %program, %program* %1, i32 0, i32 0
  %759 = getelementptr inbounds [5 x i8], [5 x i8]* @string_literal_125, i32 0, i32 0
  %760 = zext i32 4 to i64
  %761 = call ccc i8* @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %761, i8* %759, i64 %760, i1 0)
  %762 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %762, i32 4, i8* %761)
  %763 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %758, %symbol_t* %762)
  %764 = getelementptr %program, %program* %1, i32 0, i32 0
  %765 = getelementptr inbounds [41 x i8], [41 x i8]* @string_literal_126, i32 0, i32 0
  %766 = zext i32 40 to i64
  %767 = call ccc i8* @malloc(i32 40)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %767, i8* %765, i64 %766, i1 0)
  %768 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %768, i32 40, i8* %767)
  %769 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %764, %symbol_t* %768)
  %770 = getelementptr %program, %program* %1, i32 0, i32 0
  %771 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_127, i32 0, i32 0
  %772 = zext i32 11 to i64
  %773 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %773, i8* %771, i64 %772, i1 0)
  %774 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %774, i32 11, i8* %773)
  %775 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %770, %symbol_t* %774)
  %776 = getelementptr %program, %program* %1, i32 0, i32 0
  %777 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_128, i32 0, i32 0
  %778 = zext i32 11 to i64
  %779 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %779, i8* %777, i64 %778, i1 0)
  %780 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %780, i32 11, i8* %779)
  %781 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %776, %symbol_t* %780)
  %782 = getelementptr %program, %program* %1, i32 0, i32 0
  %783 = getelementptr inbounds [34 x i8], [34 x i8]* @string_literal_129, i32 0, i32 0
  %784 = zext i32 33 to i64
  %785 = call ccc i8* @malloc(i32 33)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %785, i8* %783, i64 %784, i1 0)
  %786 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %786, i32 33, i8* %785)
  %787 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %782, %symbol_t* %786)
  %788 = getelementptr %program, %program* %1, i32 0, i32 0
  %789 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_130, i32 0, i32 0
  %790 = zext i32 3 to i64
  %791 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %791, i8* %789, i64 %790, i1 0)
  %792 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %792, i32 3, i8* %791)
  %793 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %788, %symbol_t* %792)
  %794 = getelementptr %program, %program* %1, i32 0, i32 0
  %795 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_131, i32 0, i32 0
  %796 = zext i32 15 to i64
  %797 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %797, i8* %795, i64 %796, i1 0)
  %798 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %798, i32 15, i8* %797)
  %799 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %794, %symbol_t* %798)
  %800 = getelementptr %program, %program* %1, i32 0, i32 0
  %801 = getelementptr inbounds [24 x i8], [24 x i8]* @string_literal_132, i32 0, i32 0
  %802 = zext i32 23 to i64
  %803 = call ccc i8* @malloc(i32 23)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %803, i8* %801, i64 %802, i1 0)
  %804 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %804, i32 23, i8* %803)
  %805 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %800, %symbol_t* %804)
  %806 = getelementptr %program, %program* %1, i32 0, i32 0
  %807 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_133, i32 0, i32 0
  %808 = zext i32 3 to i64
  %809 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %809, i8* %807, i64 %808, i1 0)
  %810 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %810, i32 3, i8* %809)
  %811 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %806, %symbol_t* %810)
  %812 = getelementptr %program, %program* %1, i32 0, i32 0
  %813 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_134, i32 0, i32 0
  %814 = zext i32 3 to i64
  %815 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %815, i8* %813, i64 %814, i1 0)
  %816 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %816, i32 3, i8* %815)
  %817 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %812, %symbol_t* %816)
  %818 = getelementptr %program, %program* %1, i32 0, i32 0
  %819 = getelementptr inbounds [47 x i8], [47 x i8]* @string_literal_135, i32 0, i32 0
  %820 = zext i32 46 to i64
  %821 = call ccc i8* @malloc(i32 46)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %821, i8* %819, i64 %820, i1 0)
  %822 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %822, i32 46, i8* %821)
  %823 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %818, %symbol_t* %822)
  %824 = getelementptr %program, %program* %1, i32 0, i32 0
  %825 = getelementptr inbounds [19 x i8], [19 x i8]* @string_literal_136, i32 0, i32 0
  %826 = zext i32 18 to i64
  %827 = call ccc i8* @malloc(i32 18)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %827, i8* %825, i64 %826, i1 0)
  %828 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %828, i32 18, i8* %827)
  %829 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %824, %symbol_t* %828)
  %830 = getelementptr %program, %program* %1, i32 0, i32 0
  %831 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_137, i32 0, i32 0
  %832 = zext i32 9 to i64
  %833 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %833, i8* %831, i64 %832, i1 0)
  %834 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %834, i32 9, i8* %833)
  %835 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %830, %symbol_t* %834)
  %836 = getelementptr %program, %program* %1, i32 0, i32 0
  %837 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_138, i32 0, i32 0
  %838 = zext i32 6 to i64
  %839 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %839, i8* %837, i64 %838, i1 0)
  %840 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %840, i32 6, i8* %839)
  %841 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %836, %symbol_t* %840)
  %842 = getelementptr %program, %program* %1, i32 0, i32 0
  %843 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_139, i32 0, i32 0
  %844 = zext i32 3 to i64
  %845 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %845, i8* %843, i64 %844, i1 0)
  %846 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %846, i32 3, i8* %845)
  %847 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %842, %symbol_t* %846)
  %848 = getelementptr %program, %program* %1, i32 0, i32 0
  %849 = getelementptr inbounds [22 x i8], [22 x i8]* @string_literal_140, i32 0, i32 0
  %850 = zext i32 21 to i64
  %851 = call ccc i8* @malloc(i32 21)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %851, i8* %849, i64 %850, i1 0)
  %852 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %852, i32 21, i8* %851)
  %853 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %848, %symbol_t* %852)
  %854 = getelementptr %program, %program* %1, i32 0, i32 0
  %855 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_141, i32 0, i32 0
  %856 = zext i32 3 to i64
  %857 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %857, i8* %855, i64 %856, i1 0)
  %858 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %858, i32 3, i8* %857)
  %859 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %854, %symbol_t* %858)
  %860 = getelementptr %program, %program* %1, i32 0, i32 0
  %861 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_142, i32 0, i32 0
  %862 = zext i32 12 to i64
  %863 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %863, i8* %861, i64 %862, i1 0)
  %864 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %864, i32 12, i8* %863)
  %865 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %860, %symbol_t* %864)
  %866 = getelementptr %program, %program* %1, i32 0, i32 0
  %867 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_143, i32 0, i32 0
  %868 = zext i32 11 to i64
  %869 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %869, i8* %867, i64 %868, i1 0)
  %870 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %870, i32 11, i8* %869)
  %871 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %866, %symbol_t* %870)
  %872 = getelementptr %program, %program* %1, i32 0, i32 0
  %873 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_144, i32 0, i32 0
  %874 = zext i32 7 to i64
  %875 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %875, i8* %873, i64 %874, i1 0)
  %876 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %876, i32 7, i8* %875)
  %877 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %872, %symbol_t* %876)
  %878 = getelementptr %program, %program* %1, i32 0, i32 0
  %879 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_145, i32 0, i32 0
  %880 = zext i32 9 to i64
  %881 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %881, i8* %879, i64 %880, i1 0)
  %882 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %882, i32 9, i8* %881)
  %883 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %878, %symbol_t* %882)
  %884 = getelementptr %program, %program* %1, i32 0, i32 0
  %885 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_146, i32 0, i32 0
  %886 = zext i32 8 to i64
  %887 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %887, i8* %885, i64 %886, i1 0)
  %888 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %888, i32 8, i8* %887)
  %889 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %884, %symbol_t* %888)
  %890 = getelementptr %program, %program* %1, i32 0, i32 0
  %891 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_147, i32 0, i32 0
  %892 = zext i32 10 to i64
  %893 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %893, i8* %891, i64 %892, i1 0)
  %894 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %894, i32 10, i8* %893)
  %895 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %890, %symbol_t* %894)
  %896 = getelementptr %program, %program* %1, i32 0, i32 0
  %897 = getelementptr inbounds [14 x i8], [14 x i8]* @string_literal_148, i32 0, i32 0
  %898 = zext i32 13 to i64
  %899 = call ccc i8* @malloc(i32 13)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %899, i8* %897, i64 %898, i1 0)
  %900 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %900, i32 13, i8* %899)
  %901 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %896, %symbol_t* %900)
  %902 = getelementptr %program, %program* %1, i32 0, i32 0
  %903 = getelementptr inbounds [5 x i8], [5 x i8]* @string_literal_149, i32 0, i32 0
  %904 = zext i32 4 to i64
  %905 = call ccc i8* @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %905, i8* %903, i64 %904, i1 0)
  %906 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %906, i32 4, i8* %905)
  %907 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %902, %symbol_t* %906)
  %908 = getelementptr %program, %program* %1, i32 0, i32 0
  %909 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_150, i32 0, i32 0
  %910 = zext i32 11 to i64
  %911 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %911, i8* %909, i64 %910, i1 0)
  %912 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %912, i32 11, i8* %911)
  %913 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %908, %symbol_t* %912)
  %914 = getelementptr %program, %program* %1, i32 0, i32 0
  %915 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_151, i32 0, i32 0
  %916 = zext i32 5 to i64
  %917 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %917, i8* %915, i64 %916, i1 0)
  %918 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %918, i32 5, i8* %917)
  %919 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %914, %symbol_t* %918)
  %920 = getelementptr %program, %program* %1, i32 0, i32 0
  %921 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_152, i32 0, i32 0
  %922 = zext i32 7 to i64
  %923 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %923, i8* %921, i64 %922, i1 0)
  %924 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %924, i32 7, i8* %923)
  %925 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %920, %symbol_t* %924)
  %926 = getelementptr %program, %program* %1, i32 0, i32 0
  %927 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_153, i32 0, i32 0
  %928 = zext i32 5 to i64
  %929 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %929, i8* %927, i64 %928, i1 0)
  %930 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %930, i32 5, i8* %929)
  %931 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %926, %symbol_t* %930)
  %932 = getelementptr %program, %program* %1, i32 0, i32 0
  %933 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_154, i32 0, i32 0
  %934 = zext i32 6 to i64
  %935 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %935, i8* %933, i64 %934, i1 0)
  %936 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %936, i32 6, i8* %935)
  %937 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %932, %symbol_t* %936)
  %938 = getelementptr %program, %program* %1, i32 0, i32 0
  %939 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_155, i32 0, i32 0
  %940 = zext i32 8 to i64
  %941 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %941, i8* %939, i64 %940, i1 0)
  %942 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %942, i32 8, i8* %941)
  %943 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %938, %symbol_t* %942)
  %944 = getelementptr %program, %program* %1, i32 0, i32 0
  %945 = getelementptr inbounds [18 x i8], [18 x i8]* @string_literal_156, i32 0, i32 0
  %946 = zext i32 17 to i64
  %947 = call ccc i8* @malloc(i32 17)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %947, i8* %945, i64 %946, i1 0)
  %948 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %948, i32 17, i8* %947)
  %949 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %944, %symbol_t* %948)
  %950 = getelementptr %program, %program* %1, i32 0, i32 0
  %951 = getelementptr inbounds [15 x i8], [15 x i8]* @string_literal_157, i32 0, i32 0
  %952 = zext i32 14 to i64
  %953 = call ccc i8* @malloc(i32 14)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %953, i8* %951, i64 %952, i1 0)
  %954 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %954, i32 14, i8* %953)
  %955 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %950, %symbol_t* %954)
  %956 = getelementptr %program, %program* %1, i32 0, i32 0
  %957 = getelementptr inbounds [19 x i8], [19 x i8]* @string_literal_158, i32 0, i32 0
  %958 = zext i32 18 to i64
  %959 = call ccc i8* @malloc(i32 18)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %959, i8* %957, i64 %958, i1 0)
  %960 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %960, i32 18, i8* %959)
  %961 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %956, %symbol_t* %960)
  %962 = getelementptr %program, %program* %1, i32 0, i32 0
  %963 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_159, i32 0, i32 0
  %964 = zext i32 6 to i64
  %965 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %965, i8* %963, i64 %964, i1 0)
  %966 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %966, i32 6, i8* %965)
  %967 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %962, %symbol_t* %966)
  %968 = getelementptr %program, %program* %1, i32 0, i32 0
  %969 = getelementptr inbounds [18 x i8], [18 x i8]* @string_literal_160, i32 0, i32 0
  %970 = zext i32 17 to i64
  %971 = call ccc i8* @malloc(i32 17)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %971, i8* %969, i64 %970, i1 0)
  %972 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %972, i32 17, i8* %971)
  %973 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %968, %symbol_t* %972)
  %974 = getelementptr %program, %program* %1, i32 0, i32 0
  %975 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_161, i32 0, i32 0
  %976 = zext i32 12 to i64
  %977 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %977, i8* %975, i64 %976, i1 0)
  %978 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %978, i32 12, i8* %977)
  %979 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %974, %symbol_t* %978)
  %980 = getelementptr %program, %program* %1, i32 0, i32 0
  %981 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_162, i32 0, i32 0
  %982 = zext i32 7 to i64
  %983 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %983, i8* %981, i64 %982, i1 0)
  %984 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %984, i32 7, i8* %983)
  %985 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %980, %symbol_t* %984)
  %986 = getelementptr %program, %program* %1, i32 0, i32 0
  %987 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_163, i32 0, i32 0
  %988 = zext i32 12 to i64
  %989 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %989, i8* %987, i64 %988, i1 0)
  %990 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %990, i32 12, i8* %989)
  %991 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %986, %symbol_t* %990)
  %992 = getelementptr %program, %program* %1, i32 0, i32 0
  %993 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_164, i32 0, i32 0
  %994 = zext i32 12 to i64
  %995 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %995, i8* %993, i64 %994, i1 0)
  %996 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %996, i32 12, i8* %995)
  %997 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %992, %symbol_t* %996)
  %998 = getelementptr %program, %program* %1, i32 0, i32 0
  %999 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_165, i32 0, i32 0
  %1000 = zext i32 15 to i64
  %1001 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1001, i8* %999, i64 %1000, i1 0)
  %1002 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1002, i32 15, i8* %1001)
  %1003 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %998, %symbol_t* %1002)
  %1004 = getelementptr %program, %program* %1, i32 0, i32 0
  %1005 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_166, i32 0, i32 0
  %1006 = zext i32 10 to i64
  %1007 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1007, i8* %1005, i64 %1006, i1 0)
  %1008 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1008, i32 10, i8* %1007)
  %1009 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1004, %symbol_t* %1008)
  %1010 = getelementptr %program, %program* %1, i32 0, i32 0
  %1011 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_167, i32 0, i32 0
  %1012 = zext i32 11 to i64
  %1013 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1013, i8* %1011, i64 %1012, i1 0)
  %1014 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1014, i32 11, i8* %1013)
  %1015 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1010, %symbol_t* %1014)
  %1016 = getelementptr %program, %program* %1, i32 0, i32 0
  %1017 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_168, i32 0, i32 0
  %1018 = zext i32 10 to i64
  %1019 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1019, i8* %1017, i64 %1018, i1 0)
  %1020 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1020, i32 10, i8* %1019)
  %1021 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1016, %symbol_t* %1020)
  %1022 = getelementptr %program, %program* %1, i32 0, i32 0
  %1023 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_169, i32 0, i32 0
  %1024 = zext i32 3 to i64
  %1025 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1025, i8* %1023, i64 %1024, i1 0)
  %1026 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1026, i32 3, i8* %1025)
  %1027 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1022, %symbol_t* %1026)
  %1028 = getelementptr %program, %program* %1, i32 0, i32 0
  %1029 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_170, i32 0, i32 0
  %1030 = zext i32 5 to i64
  %1031 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1031, i8* %1029, i64 %1030, i1 0)
  %1032 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1032, i32 5, i8* %1031)
  %1033 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1028, %symbol_t* %1032)
  %1034 = getelementptr %program, %program* %1, i32 0, i32 0
  %1035 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_171, i32 0, i32 0
  %1036 = zext i32 10 to i64
  %1037 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1037, i8* %1035, i64 %1036, i1 0)
  %1038 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1038, i32 10, i8* %1037)
  %1039 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1034, %symbol_t* %1038)
  %1040 = getelementptr %program, %program* %1, i32 0, i32 0
  %1041 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_172, i32 0, i32 0
  %1042 = zext i32 7 to i64
  %1043 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1043, i8* %1041, i64 %1042, i1 0)
  %1044 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1044, i32 7, i8* %1043)
  %1045 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1040, %symbol_t* %1044)
  %1046 = getelementptr %program, %program* %1, i32 0, i32 0
  %1047 = getelementptr inbounds [5 x i8], [5 x i8]* @string_literal_173, i32 0, i32 0
  %1048 = zext i32 4 to i64
  %1049 = call ccc i8* @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1049, i8* %1047, i64 %1048, i1 0)
  %1050 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1050, i32 4, i8* %1049)
  %1051 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1046, %symbol_t* %1050)
  %1052 = getelementptr %program, %program* %1, i32 0, i32 0
  %1053 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_174, i32 0, i32 0
  %1054 = zext i32 7 to i64
  %1055 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1055, i8* %1053, i64 %1054, i1 0)
  %1056 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1056, i32 7, i8* %1055)
  %1057 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1052, %symbol_t* %1056)
  %1058 = getelementptr %program, %program* %1, i32 0, i32 0
  %1059 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_175, i32 0, i32 0
  %1060 = zext i32 11 to i64
  %1061 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1061, i8* %1059, i64 %1060, i1 0)
  %1062 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1062, i32 11, i8* %1061)
  %1063 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1058, %symbol_t* %1062)
  %1064 = getelementptr %program, %program* %1, i32 0, i32 0
  %1065 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_176, i32 0, i32 0
  %1066 = zext i32 15 to i64
  %1067 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1067, i8* %1065, i64 %1066, i1 0)
  %1068 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1068, i32 15, i8* %1067)
  %1069 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1064, %symbol_t* %1068)
  %1070 = getelementptr %program, %program* %1, i32 0, i32 0
  %1071 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_177, i32 0, i32 0
  %1072 = zext i32 9 to i64
  %1073 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1073, i8* %1071, i64 %1072, i1 0)
  %1074 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1074, i32 9, i8* %1073)
  %1075 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1070, %symbol_t* %1074)
  %1076 = getelementptr %program, %program* %1, i32 0, i32 0
  %1077 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_178, i32 0, i32 0
  %1078 = zext i32 12 to i64
  %1079 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1079, i8* %1077, i64 %1078, i1 0)
  %1080 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1080, i32 12, i8* %1079)
  %1081 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1076, %symbol_t* %1080)
  %1082 = getelementptr %program, %program* %1, i32 0, i32 0
  %1083 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_179, i32 0, i32 0
  %1084 = zext i32 6 to i64
  %1085 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1085, i8* %1083, i64 %1084, i1 0)
  %1086 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1086, i32 6, i8* %1085)
  %1087 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1082, %symbol_t* %1086)
  %1088 = getelementptr %program, %program* %1, i32 0, i32 0
  %1089 = getelementptr inbounds [5 x i8], [5 x i8]* @string_literal_180, i32 0, i32 0
  %1090 = zext i32 4 to i64
  %1091 = call ccc i8* @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1091, i8* %1089, i64 %1090, i1 0)
  %1092 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1092, i32 4, i8* %1091)
  %1093 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1088, %symbol_t* %1092)
  %1094 = getelementptr %program, %program* %1, i32 0, i32 0
  %1095 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_181, i32 0, i32 0
  %1096 = zext i32 7 to i64
  %1097 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1097, i8* %1095, i64 %1096, i1 0)
  %1098 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1098, i32 7, i8* %1097)
  %1099 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1094, %symbol_t* %1098)
  %1100 = getelementptr %program, %program* %1, i32 0, i32 0
  %1101 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_182, i32 0, i32 0
  %1102 = zext i32 8 to i64
  %1103 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1103, i8* %1101, i64 %1102, i1 0)
  %1104 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1104, i32 8, i8* %1103)
  %1105 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1100, %symbol_t* %1104)
  %1106 = getelementptr %program, %program* %1, i32 0, i32 0
  %1107 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_183, i32 0, i32 0
  %1108 = zext i32 5 to i64
  %1109 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1109, i8* %1107, i64 %1108, i1 0)
  %1110 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1110, i32 5, i8* %1109)
  %1111 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1106, %symbol_t* %1110)
  %1112 = getelementptr %program, %program* %1, i32 0, i32 0
  %1113 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_184, i32 0, i32 0
  %1114 = zext i32 5 to i64
  %1115 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1115, i8* %1113, i64 %1114, i1 0)
  %1116 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1116, i32 5, i8* %1115)
  %1117 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1112, %symbol_t* %1116)
  %1118 = getelementptr %program, %program* %1, i32 0, i32 0
  %1119 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_185, i32 0, i32 0
  %1120 = zext i32 7 to i64
  %1121 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1121, i8* %1119, i64 %1120, i1 0)
  %1122 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1122, i32 7, i8* %1121)
  %1123 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1118, %symbol_t* %1122)
  %1124 = getelementptr %program, %program* %1, i32 0, i32 0
  %1125 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_186, i32 0, i32 0
  %1126 = zext i32 12 to i64
  %1127 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1127, i8* %1125, i64 %1126, i1 0)
  %1128 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1128, i32 12, i8* %1127)
  %1129 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1124, %symbol_t* %1128)
  %1130 = getelementptr %program, %program* %1, i32 0, i32 0
  %1131 = getelementptr inbounds [19 x i8], [19 x i8]* @string_literal_187, i32 0, i32 0
  %1132 = zext i32 18 to i64
  %1133 = call ccc i8* @malloc(i32 18)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1133, i8* %1131, i64 %1132, i1 0)
  %1134 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1134, i32 18, i8* %1133)
  %1135 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1130, %symbol_t* %1134)
  %1136 = getelementptr %program, %program* %1, i32 0, i32 0
  %1137 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_188, i32 0, i32 0
  %1138 = zext i32 3 to i64
  %1139 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1139, i8* %1137, i64 %1138, i1 0)
  %1140 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1140, i32 3, i8* %1139)
  %1141 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1136, %symbol_t* %1140)
  %1142 = getelementptr %program, %program* %1, i32 0, i32 0
  %1143 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_189, i32 0, i32 0
  %1144 = zext i32 15 to i64
  %1145 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1145, i8* %1143, i64 %1144, i1 0)
  %1146 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1146, i32 15, i8* %1145)
  %1147 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1142, %symbol_t* %1146)
  %1148 = getelementptr %program, %program* %1, i32 0, i32 0
  %1149 = getelementptr inbounds [15 x i8], [15 x i8]* @string_literal_190, i32 0, i32 0
  %1150 = zext i32 14 to i64
  %1151 = call ccc i8* @malloc(i32 14)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1151, i8* %1149, i64 %1150, i1 0)
  %1152 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1152, i32 14, i8* %1151)
  %1153 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1148, %symbol_t* %1152)
  %1154 = getelementptr %program, %program* %1, i32 0, i32 0
  %1155 = getelementptr inbounds [14 x i8], [14 x i8]* @string_literal_191, i32 0, i32 0
  %1156 = zext i32 13 to i64
  %1157 = call ccc i8* @malloc(i32 13)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1157, i8* %1155, i64 %1156, i1 0)
  %1158 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1158, i32 13, i8* %1157)
  %1159 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1154, %symbol_t* %1158)
  %1160 = getelementptr %program, %program* %1, i32 0, i32 0
  %1161 = getelementptr inbounds [3 x i8], [3 x i8]* @string_literal_192, i32 0, i32 0
  %1162 = zext i32 2 to i64
  %1163 = call ccc i8* @malloc(i32 2)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1163, i8* %1161, i64 %1162, i1 0)
  %1164 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1164, i32 2, i8* %1163)
  %1165 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1160, %symbol_t* %1164)
  %1166 = getelementptr %program, %program* %1, i32 0, i32 0
  %1167 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_193, i32 0, i32 0
  %1168 = zext i32 15 to i64
  %1169 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1169, i8* %1167, i64 %1168, i1 0)
  %1170 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1170, i32 15, i8* %1169)
  %1171 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1166, %symbol_t* %1170)
  %1172 = getelementptr %program, %program* %1, i32 0, i32 0
  %1173 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_194, i32 0, i32 0
  %1174 = zext i32 15 to i64
  %1175 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1175, i8* %1173, i64 %1174, i1 0)
  %1176 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1176, i32 15, i8* %1175)
  %1177 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1172, %symbol_t* %1176)
  %1178 = getelementptr %program, %program* %1, i32 0, i32 0
  %1179 = getelementptr inbounds [16 x i8], [16 x i8]* @string_literal_195, i32 0, i32 0
  %1180 = zext i32 15 to i64
  %1181 = call ccc i8* @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1181, i8* %1179, i64 %1180, i1 0)
  %1182 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1182, i32 15, i8* %1181)
  %1183 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1178, %symbol_t* %1182)
  %1184 = getelementptr %program, %program* %1, i32 0, i32 0
  %1185 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_196, i32 0, i32 0
  %1186 = zext i32 3 to i64
  %1187 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1187, i8* %1185, i64 %1186, i1 0)
  %1188 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1188, i32 3, i8* %1187)
  %1189 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1184, %symbol_t* %1188)
  %1190 = getelementptr %program, %program* %1, i32 0, i32 0
  %1191 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_197, i32 0, i32 0
  %1192 = zext i32 10 to i64
  %1193 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1193, i8* %1191, i64 %1192, i1 0)
  %1194 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1194, i32 10, i8* %1193)
  %1195 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1190, %symbol_t* %1194)
  %1196 = getelementptr %program, %program* %1, i32 0, i32 0
  %1197 = getelementptr inbounds [5 x i8], [5 x i8]* @string_literal_198, i32 0, i32 0
  %1198 = zext i32 4 to i64
  %1199 = call ccc i8* @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1199, i8* %1197, i64 %1198, i1 0)
  %1200 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1200, i32 4, i8* %1199)
  %1201 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1196, %symbol_t* %1200)
  %1202 = getelementptr %program, %program* %1, i32 0, i32 0
  %1203 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_199, i32 0, i32 0
  %1204 = zext i32 3 to i64
  %1205 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1205, i8* %1203, i64 %1204, i1 0)
  %1206 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1206, i32 3, i8* %1205)
  %1207 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1202, %symbol_t* %1206)
  %1208 = getelementptr %program, %program* %1, i32 0, i32 0
  %1209 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_200, i32 0, i32 0
  %1210 = zext i32 3 to i64
  %1211 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1211, i8* %1209, i64 %1210, i1 0)
  %1212 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1212, i32 3, i8* %1211)
  %1213 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1208, %symbol_t* %1212)
  %1214 = getelementptr %program, %program* %1, i32 0, i32 0
  %1215 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_201, i32 0, i32 0
  %1216 = zext i32 6 to i64
  %1217 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1217, i8* %1215, i64 %1216, i1 0)
  %1218 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1218, i32 6, i8* %1217)
  %1219 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1214, %symbol_t* %1218)
  %1220 = getelementptr %program, %program* %1, i32 0, i32 0
  %1221 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_202, i32 0, i32 0
  %1222 = zext i32 3 to i64
  %1223 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1223, i8* %1221, i64 %1222, i1 0)
  %1224 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1224, i32 3, i8* %1223)
  %1225 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1220, %symbol_t* %1224)
  %1226 = getelementptr %program, %program* %1, i32 0, i32 0
  %1227 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_203, i32 0, i32 0
  %1228 = zext i32 7 to i64
  %1229 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1229, i8* %1227, i64 %1228, i1 0)
  %1230 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1230, i32 7, i8* %1229)
  %1231 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1226, %symbol_t* %1230)
  %1232 = getelementptr %program, %program* %1, i32 0, i32 0
  %1233 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_204, i32 0, i32 0
  %1234 = zext i32 5 to i64
  %1235 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1235, i8* %1233, i64 %1234, i1 0)
  %1236 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1236, i32 5, i8* %1235)
  %1237 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1232, %symbol_t* %1236)
  %1238 = getelementptr %program, %program* %1, i32 0, i32 0
  %1239 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_205, i32 0, i32 0
  %1240 = zext i32 8 to i64
  %1241 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1241, i8* %1239, i64 %1240, i1 0)
  %1242 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1242, i32 8, i8* %1241)
  %1243 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1238, %symbol_t* %1242)
  %1244 = getelementptr %program, %program* %1, i32 0, i32 0
  %1245 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_206, i32 0, i32 0
  %1246 = zext i32 10 to i64
  %1247 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1247, i8* %1245, i64 %1246, i1 0)
  %1248 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1248, i32 10, i8* %1247)
  %1249 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1244, %symbol_t* %1248)
  %1250 = getelementptr %program, %program* %1, i32 0, i32 0
  %1251 = getelementptr inbounds [5 x i8], [5 x i8]* @string_literal_207, i32 0, i32 0
  %1252 = zext i32 4 to i64
  %1253 = call ccc i8* @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1253, i8* %1251, i64 %1252, i1 0)
  %1254 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1254, i32 4, i8* %1253)
  %1255 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1250, %symbol_t* %1254)
  %1256 = getelementptr %program, %program* %1, i32 0, i32 0
  %1257 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_208, i32 0, i32 0
  %1258 = zext i32 5 to i64
  %1259 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1259, i8* %1257, i64 %1258, i1 0)
  %1260 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1260, i32 5, i8* %1259)
  %1261 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1256, %symbol_t* %1260)
  %1262 = getelementptr %program, %program* %1, i32 0, i32 0
  %1263 = getelementptr inbounds [30 x i8], [30 x i8]* @string_literal_209, i32 0, i32 0
  %1264 = zext i32 29 to i64
  %1265 = call ccc i8* @malloc(i32 29)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1265, i8* %1263, i64 %1264, i1 0)
  %1266 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1266, i32 29, i8* %1265)
  %1267 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1262, %symbol_t* %1266)
  %1268 = getelementptr %program, %program* %1, i32 0, i32 0
  %1269 = getelementptr inbounds [27 x i8], [27 x i8]* @string_literal_210, i32 0, i32 0
  %1270 = zext i32 26 to i64
  %1271 = call ccc i8* @malloc(i32 26)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1271, i8* %1269, i64 %1270, i1 0)
  %1272 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1272, i32 26, i8* %1271)
  %1273 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1268, %symbol_t* %1272)
  %1274 = getelementptr %program, %program* %1, i32 0, i32 0
  %1275 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_211, i32 0, i32 0
  %1276 = zext i32 5 to i64
  %1277 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1277, i8* %1275, i64 %1276, i1 0)
  %1278 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1278, i32 5, i8* %1277)
  %1279 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1274, %symbol_t* %1278)
  %1280 = getelementptr %program, %program* %1, i32 0, i32 0
  %1281 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_212, i32 0, i32 0
  %1282 = zext i32 3 to i64
  %1283 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1283, i8* %1281, i64 %1282, i1 0)
  %1284 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1284, i32 3, i8* %1283)
  %1285 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1280, %symbol_t* %1284)
  %1286 = getelementptr %program, %program* %1, i32 0, i32 0
  %1287 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_213, i32 0, i32 0
  %1288 = zext i32 11 to i64
  %1289 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1289, i8* %1287, i64 %1288, i1 0)
  %1290 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1290, i32 11, i8* %1289)
  %1291 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1286, %symbol_t* %1290)
  %1292 = getelementptr %program, %program* %1, i32 0, i32 0
  %1293 = getelementptr inbounds [5 x i8], [5 x i8]* @string_literal_214, i32 0, i32 0
  %1294 = zext i32 4 to i64
  %1295 = call ccc i8* @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1295, i8* %1293, i64 %1294, i1 0)
  %1296 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1296, i32 4, i8* %1295)
  %1297 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1292, %symbol_t* %1296)
  %1298 = getelementptr %program, %program* %1, i32 0, i32 0
  %1299 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_215, i32 0, i32 0
  %1300 = zext i32 3 to i64
  %1301 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1301, i8* %1299, i64 %1300, i1 0)
  %1302 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1302, i32 3, i8* %1301)
  %1303 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1298, %symbol_t* %1302)
  %1304 = getelementptr %program, %program* %1, i32 0, i32 0
  %1305 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_216, i32 0, i32 0
  %1306 = zext i32 5 to i64
  %1307 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1307, i8* %1305, i64 %1306, i1 0)
  %1308 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1308, i32 5, i8* %1307)
  %1309 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1304, %symbol_t* %1308)
  %1310 = getelementptr %program, %program* %1, i32 0, i32 0
  %1311 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_217, i32 0, i32 0
  %1312 = zext i32 6 to i64
  %1313 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1313, i8* %1311, i64 %1312, i1 0)
  %1314 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1314, i32 6, i8* %1313)
  %1315 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1310, %symbol_t* %1314)
  %1316 = getelementptr %program, %program* %1, i32 0, i32 0
  %1317 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_218, i32 0, i32 0
  %1318 = zext i32 3 to i64
  %1319 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1319, i8* %1317, i64 %1318, i1 0)
  %1320 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1320, i32 3, i8* %1319)
  %1321 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1316, %symbol_t* %1320)
  %1322 = getelementptr %program, %program* %1, i32 0, i32 0
  %1323 = getelementptr inbounds [23 x i8], [23 x i8]* @string_literal_219, i32 0, i32 0
  %1324 = zext i32 22 to i64
  %1325 = call ccc i8* @malloc(i32 22)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1325, i8* %1323, i64 %1324, i1 0)
  %1326 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1326, i32 22, i8* %1325)
  %1327 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1322, %symbol_t* %1326)
  %1328 = getelementptr %program, %program* %1, i32 0, i32 0
  %1329 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_220, i32 0, i32 0
  %1330 = zext i32 12 to i64
  %1331 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1331, i8* %1329, i64 %1330, i1 0)
  %1332 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1332, i32 12, i8* %1331)
  %1333 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1328, %symbol_t* %1332)
  %1334 = getelementptr %program, %program* %1, i32 0, i32 0
  %1335 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_221, i32 0, i32 0
  %1336 = zext i32 8 to i64
  %1337 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1337, i8* %1335, i64 %1336, i1 0)
  %1338 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1338, i32 8, i8* %1337)
  %1339 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1334, %symbol_t* %1338)
  %1340 = getelementptr %program, %program* %1, i32 0, i32 0
  %1341 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_222, i32 0, i32 0
  %1342 = zext i32 3 to i64
  %1343 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1343, i8* %1341, i64 %1342, i1 0)
  %1344 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1344, i32 3, i8* %1343)
  %1345 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1340, %symbol_t* %1344)
  %1346 = getelementptr %program, %program* %1, i32 0, i32 0
  %1347 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_223, i32 0, i32 0
  %1348 = zext i32 8 to i64
  %1349 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1349, i8* %1347, i64 %1348, i1 0)
  %1350 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1350, i32 8, i8* %1349)
  %1351 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1346, %symbol_t* %1350)
  %1352 = getelementptr %program, %program* %1, i32 0, i32 0
  %1353 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_224, i32 0, i32 0
  %1354 = zext i32 10 to i64
  %1355 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1355, i8* %1353, i64 %1354, i1 0)
  %1356 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1356, i32 10, i8* %1355)
  %1357 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1352, %symbol_t* %1356)
  %1358 = getelementptr %program, %program* %1, i32 0, i32 0
  %1359 = getelementptr inbounds [4 x i8], [4 x i8]* @string_literal_225, i32 0, i32 0
  %1360 = zext i32 3 to i64
  %1361 = call ccc i8* @malloc(i32 3)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1361, i8* %1359, i64 %1360, i1 0)
  %1362 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1362, i32 3, i8* %1361)
  %1363 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1358, %symbol_t* %1362)
  %1364 = getelementptr %program, %program* %1, i32 0, i32 0
  %1365 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_226, i32 0, i32 0
  %1366 = zext i32 8 to i64
  %1367 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1367, i8* %1365, i64 %1366, i1 0)
  %1368 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1368, i32 8, i8* %1367)
  %1369 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1364, %symbol_t* %1368)
  %1370 = getelementptr %program, %program* %1, i32 0, i32 0
  %1371 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_227, i32 0, i32 0
  %1372 = zext i32 10 to i64
  %1373 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1373, i8* %1371, i64 %1372, i1 0)
  %1374 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1374, i32 10, i8* %1373)
  %1375 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1370, %symbol_t* %1374)
  %1376 = getelementptr %program, %program* %1, i32 0, i32 0
  %1377 = getelementptr inbounds [5 x i8], [5 x i8]* @string_literal_228, i32 0, i32 0
  %1378 = zext i32 4 to i64
  %1379 = call ccc i8* @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1379, i8* %1377, i64 %1378, i1 0)
  %1380 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1380, i32 4, i8* %1379)
  %1381 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1376, %symbol_t* %1380)
  %1382 = getelementptr %program, %program* %1, i32 0, i32 0
  %1383 = getelementptr inbounds [12 x i8], [12 x i8]* @string_literal_229, i32 0, i32 0
  %1384 = zext i32 11 to i64
  %1385 = call ccc i8* @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1385, i8* %1383, i64 %1384, i1 0)
  %1386 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1386, i32 11, i8* %1385)
  %1387 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1382, %symbol_t* %1386)
  %1388 = getelementptr %program, %program* %1, i32 0, i32 0
  %1389 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_230, i32 0, i32 0
  %1390 = zext i32 10 to i64
  %1391 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1391, i8* %1389, i64 %1390, i1 0)
  %1392 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1392, i32 10, i8* %1391)
  %1393 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1388, %symbol_t* %1392)
  %1394 = getelementptr %program, %program* %1, i32 0, i32 0
  %1395 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_231, i32 0, i32 0
  %1396 = zext i32 8 to i64
  %1397 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1397, i8* %1395, i64 %1396, i1 0)
  %1398 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1398, i32 8, i8* %1397)
  %1399 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1394, %symbol_t* %1398)
  %1400 = getelementptr %program, %program* %1, i32 0, i32 0
  %1401 = getelementptr inbounds [22 x i8], [22 x i8]* @string_literal_232, i32 0, i32 0
  %1402 = zext i32 21 to i64
  %1403 = call ccc i8* @malloc(i32 21)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1403, i8* %1401, i64 %1402, i1 0)
  %1404 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1404, i32 21, i8* %1403)
  %1405 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1400, %symbol_t* %1404)
  %1406 = getelementptr %program, %program* %1, i32 0, i32 0
  %1407 = getelementptr inbounds [11 x i8], [11 x i8]* @string_literal_233, i32 0, i32 0
  %1408 = zext i32 10 to i64
  %1409 = call ccc i8* @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1409, i8* %1407, i64 %1408, i1 0)
  %1410 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1410, i32 10, i8* %1409)
  %1411 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1406, %symbol_t* %1410)
  %1412 = getelementptr %program, %program* %1, i32 0, i32 0
  %1413 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_234, i32 0, i32 0
  %1414 = zext i32 6 to i64
  %1415 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1415, i8* %1413, i64 %1414, i1 0)
  %1416 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1416, i32 6, i8* %1415)
  %1417 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1412, %symbol_t* %1416)
  %1418 = getelementptr %program, %program* %1, i32 0, i32 0
  %1419 = getelementptr inbounds [7 x i8], [7 x i8]* @string_literal_235, i32 0, i32 0
  %1420 = zext i32 6 to i64
  %1421 = call ccc i8* @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1421, i8* %1419, i64 %1420, i1 0)
  %1422 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1422, i32 6, i8* %1421)
  %1423 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1418, %symbol_t* %1422)
  %1424 = getelementptr %program, %program* %1, i32 0, i32 0
  %1425 = getelementptr inbounds [9 x i8], [9 x i8]* @string_literal_236, i32 0, i32 0
  %1426 = zext i32 8 to i64
  %1427 = call ccc i8* @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1427, i8* %1425, i64 %1426, i1 0)
  %1428 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1428, i32 8, i8* %1427)
  %1429 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1424, %symbol_t* %1428)
  %1430 = getelementptr %program, %program* %1, i32 0, i32 0
  %1431 = getelementptr inbounds [13 x i8], [13 x i8]* @string_literal_237, i32 0, i32 0
  %1432 = zext i32 12 to i64
  %1433 = call ccc i8* @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1433, i8* %1431, i64 %1432, i1 0)
  %1434 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1434, i32 12, i8* %1433)
  %1435 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1430, %symbol_t* %1434)
  %1436 = getelementptr %program, %program* %1, i32 0, i32 0
  %1437 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_238, i32 0, i32 0
  %1438 = zext i32 5 to i64
  %1439 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1439, i8* %1437, i64 %1438, i1 0)
  %1440 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1440, i32 5, i8* %1439)
  %1441 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1436, %symbol_t* %1440)
  %1442 = getelementptr %program, %program* %1, i32 0, i32 0
  %1443 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_239, i32 0, i32 0
  %1444 = zext i32 5 to i64
  %1445 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1445, i8* %1443, i64 %1444, i1 0)
  %1446 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1446, i32 5, i8* %1445)
  %1447 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1442, %symbol_t* %1446)
  %1448 = getelementptr %program, %program* %1, i32 0, i32 0
  %1449 = getelementptr inbounds [10 x i8], [10 x i8]* @string_literal_240, i32 0, i32 0
  %1450 = zext i32 9 to i64
  %1451 = call ccc i8* @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1451, i8* %1449, i64 %1450, i1 0)
  %1452 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1452, i32 9, i8* %1451)
  %1453 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1448, %symbol_t* %1452)
  %1454 = getelementptr %program, %program* %1, i32 0, i32 0
  %1455 = getelementptr inbounds [14 x i8], [14 x i8]* @string_literal_241, i32 0, i32 0
  %1456 = zext i32 13 to i64
  %1457 = call ccc i8* @malloc(i32 13)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1457, i8* %1455, i64 %1456, i1 0)
  %1458 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1458, i32 13, i8* %1457)
  %1459 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1454, %symbol_t* %1458)
  %1460 = getelementptr %program, %program* %1, i32 0, i32 0
  %1461 = getelementptr inbounds [8 x i8], [8 x i8]* @string_literal_242, i32 0, i32 0
  %1462 = zext i32 7 to i64
  %1463 = call ccc i8* @malloc(i32 7)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1463, i8* %1461, i64 %1462, i1 0)
  %1464 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1464, i32 7, i8* %1463)
  %1465 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1460, %symbol_t* %1464)
  %1466 = getelementptr %program, %program* %1, i32 0, i32 0
  %1467 = getelementptr inbounds [6 x i8], [6 x i8]* @string_literal_243, i32 0, i32 0
  %1468 = zext i32 5 to i64
  %1469 = call ccc i8* @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %1469, i8* %1467, i64 %1468, i1 0)
  %1470 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %1470, i32 5, i8* %1469)
  %1471 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %1466, %symbol_t* %1470)
  ret %program* %1
}

define external ccc void @eclair_program_destroy(%program* %arg_0) "wasm-export-name"="eclair_program_destroy" {
start:
  %0 = getelementptr %program, %program* %arg_0, i32 0, i32 0
  call ccc void @eclair_symbol_table_destroy(%symbol_table* %0)
  %1 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  call ccc void @eclair_btree_destroy_0(%btree_t_0* %1)
  %2 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_destroy_1(%btree_t_1* %2)
  %3 = getelementptr %program, %program* %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_destroy_2(%btree_t_2* %3)
  %4 = getelementptr %program, %program* %arg_0, i32 0, i32 4
  call ccc void @eclair_btree_destroy_2(%btree_t_2* %4)
  %5 = getelementptr %program, %program* %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_destroy_2(%btree_t_2* %5)
  %6 = bitcast %program* %arg_0 to i8*
  call ccc void @free(i8* %6)
  ret void
}

define external ccc void @eclair_program_run(%program* %arg_0) "wasm-export-name"="eclair_program_run" {
start:
  %0 = alloca [2 x i32], i32 1
  %1 = getelementptr [2 x i32], [2 x i32]* %0, i32 0, i32 0
  store i32 243, i32* %1
  %2 = getelementptr [2 x i32], [2 x i32]* %0, i32 0, i32 1
  store i32 125, i32* %2
  %3 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %4 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %3, [2 x i32]* %0)
  %5 = alloca [2 x i32], i32 1
  %6 = getelementptr [2 x i32], [2 x i32]* %5, i32 0, i32 0
  store i32 242, i32* %6
  %7 = getelementptr [2 x i32], [2 x i32]* %5, i32 0, i32 1
  store i32 116, i32* %7
  %8 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %9 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %8, [2 x i32]* %5)
  %10 = alloca [2 x i32], i32 1
  %11 = getelementptr [2 x i32], [2 x i32]* %10, i32 0, i32 0
  store i32 241, i32* %11
  %12 = getelementptr [2 x i32], [2 x i32]* %10, i32 0, i32 1
  store i32 116, i32* %12
  %13 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %14 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %13, [2 x i32]* %10)
  %15 = alloca [2 x i32], i32 1
  %16 = getelementptr [2 x i32], [2 x i32]* %15, i32 0, i32 0
  store i32 240, i32* %16
  %17 = getelementptr [2 x i32], [2 x i32]* %15, i32 0, i32 1
  store i32 107, i32* %17
  %18 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %19 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %18, [2 x i32]* %15)
  %20 = alloca [2 x i32], i32 1
  %21 = getelementptr [2 x i32], [2 x i32]* %20, i32 0, i32 0
  store i32 239, i32* %21
  %22 = getelementptr [2 x i32], [2 x i32]* %20, i32 0, i32 1
  store i32 107, i32* %22
  %23 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %24 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %23, [2 x i32]* %20)
  %25 = alloca [2 x i32], i32 1
  %26 = getelementptr [2 x i32], [2 x i32]* %25, i32 0, i32 0
  store i32 238, i32* %26
  %27 = getelementptr [2 x i32], [2 x i32]* %25, i32 0, i32 1
  store i32 107, i32* %27
  %28 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %29 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %28, [2 x i32]* %25)
  %30 = alloca [2 x i32], i32 1
  %31 = getelementptr [2 x i32], [2 x i32]* %30, i32 0, i32 0
  store i32 237, i32* %31
  %32 = getelementptr [2 x i32], [2 x i32]* %30, i32 0, i32 1
  store i32 44, i32* %32
  %33 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %34 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %33, [2 x i32]* %30)
  %35 = alloca [2 x i32], i32 1
  %36 = getelementptr [2 x i32], [2 x i32]* %35, i32 0, i32 0
  store i32 236, i32* %36
  %37 = getelementptr [2 x i32], [2 x i32]* %35, i32 0, i32 1
  store i32 113, i32* %37
  %38 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %39 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %38, [2 x i32]* %35)
  %40 = alloca [2 x i32], i32 1
  %41 = getelementptr [2 x i32], [2 x i32]* %40, i32 0, i32 0
  store i32 235, i32* %41
  %42 = getelementptr [2 x i32], [2 x i32]* %40, i32 0, i32 1
  store i32 113, i32* %42
  %43 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %44 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %43, [2 x i32]* %40)
  %45 = alloca [2 x i32], i32 1
  %46 = getelementptr [2 x i32], [2 x i32]* %45, i32 0, i32 0
  store i32 234, i32* %46
  %47 = getelementptr [2 x i32], [2 x i32]* %45, i32 0, i32 1
  store i32 113, i32* %47
  %48 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %49 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %48, [2 x i32]* %45)
  %50 = alloca [2 x i32], i32 1
  %51 = getelementptr [2 x i32], [2 x i32]* %50, i32 0, i32 0
  store i32 112, i32* %51
  %52 = getelementptr [2 x i32], [2 x i32]* %50, i32 0, i32 1
  store i32 113, i32* %52
  %53 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %54 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %53, [2 x i32]* %50)
  %55 = alloca [2 x i32], i32 1
  %56 = getelementptr [2 x i32], [2 x i32]* %55, i32 0, i32 0
  store i32 233, i32* %56
  %57 = getelementptr [2 x i32], [2 x i32]* %55, i32 0, i32 1
  store i32 113, i32* %57
  %58 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %59 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %58, [2 x i32]* %55)
  %60 = alloca [2 x i32], i32 1
  %61 = getelementptr [2 x i32], [2 x i32]* %60, i32 0, i32 0
  store i32 232, i32* %61
  %62 = getelementptr [2 x i32], [2 x i32]* %60, i32 0, i32 1
  store i32 41, i32* %62
  %63 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %64 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %63, [2 x i32]* %60)
  %65 = alloca [2 x i32], i32 1
  %66 = getelementptr [2 x i32], [2 x i32]* %65, i32 0, i32 0
  store i32 231, i32* %66
  %67 = getelementptr [2 x i32], [2 x i32]* %65, i32 0, i32 1
  store i32 89, i32* %67
  %68 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %69 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %68, [2 x i32]* %65)
  %70 = alloca [2 x i32], i32 1
  %71 = getelementptr [2 x i32], [2 x i32]* %70, i32 0, i32 0
  store i32 230, i32* %71
  %72 = getelementptr [2 x i32], [2 x i32]* %70, i32 0, i32 1
  store i32 26, i32* %72
  %73 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %74 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %73, [2 x i32]* %70)
  %75 = alloca [2 x i32], i32 1
  %76 = getelementptr [2 x i32], [2 x i32]* %75, i32 0, i32 0
  store i32 229, i32* %76
  %77 = getelementptr [2 x i32], [2 x i32]* %75, i32 0, i32 1
  store i32 53, i32* %77
  %78 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %79 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %78, [2 x i32]* %75)
  %80 = alloca [2 x i32], i32 1
  %81 = getelementptr [2 x i32], [2 x i32]* %80, i32 0, i32 0
  store i32 228, i32* %81
  %82 = getelementptr [2 x i32], [2 x i32]* %80, i32 0, i32 1
  store i32 53, i32* %82
  %83 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %84 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %83, [2 x i32]* %80)
  %85 = alloca [2 x i32], i32 1
  %86 = getelementptr [2 x i32], [2 x i32]* %85, i32 0, i32 0
  store i32 227, i32* %86
  %87 = getelementptr [2 x i32], [2 x i32]* %85, i32 0, i32 1
  store i32 32, i32* %87
  %88 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %89 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %88, [2 x i32]* %85)
  %90 = alloca [2 x i32], i32 1
  %91 = getelementptr [2 x i32], [2 x i32]* %90, i32 0, i32 0
  store i32 226, i32* %91
  %92 = getelementptr [2 x i32], [2 x i32]* %90, i32 0, i32 1
  store i32 86, i32* %92
  %93 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %94 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %93, [2 x i32]* %90)
  %95 = alloca [2 x i32], i32 1
  %96 = getelementptr [2 x i32], [2 x i32]* %95, i32 0, i32 0
  store i32 225, i32* %96
  %97 = getelementptr [2 x i32], [2 x i32]* %95, i32 0, i32 1
  store i32 86, i32* %97
  %98 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %99 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %98, [2 x i32]* %95)
  %100 = alloca [2 x i32], i32 1
  %101 = getelementptr [2 x i32], [2 x i32]* %100, i32 0, i32 0
  store i32 224, i32* %101
  %102 = getelementptr [2 x i32], [2 x i32]* %100, i32 0, i32 1
  store i32 35, i32* %102
  %103 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %104 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %103, [2 x i32]* %100)
  %105 = alloca [2 x i32], i32 1
  %106 = getelementptr [2 x i32], [2 x i32]* %105, i32 0, i32 0
  store i32 223, i32* %106
  %107 = getelementptr [2 x i32], [2 x i32]* %105, i32 0, i32 1
  store i32 35, i32* %107
  %108 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %109 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %108, [2 x i32]* %105)
  %110 = alloca [2 x i32], i32 1
  %111 = getelementptr [2 x i32], [2 x i32]* %110, i32 0, i32 0
  store i32 222, i32* %111
  %112 = getelementptr [2 x i32], [2 x i32]* %110, i32 0, i32 1
  store i32 20, i32* %112
  %113 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %114 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %113, [2 x i32]* %110)
  %115 = alloca [2 x i32], i32 1
  %116 = getelementptr [2 x i32], [2 x i32]* %115, i32 0, i32 0
  store i32 221, i32* %116
  %117 = getelementptr [2 x i32], [2 x i32]* %115, i32 0, i32 1
  store i32 20, i32* %117
  %118 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %119 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %118, [2 x i32]* %115)
  %120 = alloca [2 x i32], i32 1
  %121 = getelementptr [2 x i32], [2 x i32]* %120, i32 0, i32 0
  store i32 220, i32* %121
  %122 = getelementptr [2 x i32], [2 x i32]* %120, i32 0, i32 1
  store i32 20, i32* %122
  %123 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %124 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %123, [2 x i32]* %120)
  %125 = alloca [2 x i32], i32 1
  %126 = getelementptr [2 x i32], [2 x i32]* %125, i32 0, i32 0
  store i32 219, i32* %126
  %127 = getelementptr [2 x i32], [2 x i32]* %125, i32 0, i32 1
  store i32 20, i32* %127
  %128 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %129 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %128, [2 x i32]* %125)
  %130 = alloca [2 x i32], i32 1
  %131 = getelementptr [2 x i32], [2 x i32]* %130, i32 0, i32 0
  store i32 218, i32* %131
  %132 = getelementptr [2 x i32], [2 x i32]* %130, i32 0, i32 1
  store i32 14, i32* %132
  %133 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %134 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %133, [2 x i32]* %130)
  %135 = alloca [2 x i32], i32 1
  %136 = getelementptr [2 x i32], [2 x i32]* %135, i32 0, i32 0
  store i32 217, i32* %136
  %137 = getelementptr [2 x i32], [2 x i32]* %135, i32 0, i32 1
  store i32 14, i32* %137
  %138 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %139 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %138, [2 x i32]* %135)
  %140 = alloca [2 x i32], i32 1
  %141 = getelementptr [2 x i32], [2 x i32]* %140, i32 0, i32 0
  store i32 216, i32* %141
  %142 = getelementptr [2 x i32], [2 x i32]* %140, i32 0, i32 1
  store i32 14, i32* %142
  %143 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %144 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %143, [2 x i32]* %140)
  %145 = alloca [2 x i32], i32 1
  %146 = getelementptr [2 x i32], [2 x i32]* %145, i32 0, i32 0
  store i32 215, i32* %146
  %147 = getelementptr [2 x i32], [2 x i32]* %145, i32 0, i32 1
  store i32 213, i32* %147
  %148 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %149 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %148, [2 x i32]* %145)
  %150 = alloca [2 x i32], i32 1
  %151 = getelementptr [2 x i32], [2 x i32]* %150, i32 0, i32 0
  store i32 214, i32* %151
  %152 = getelementptr [2 x i32], [2 x i32]* %150, i32 0, i32 1
  store i32 213, i32* %152
  %153 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %154 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %153, [2 x i32]* %150)
  %155 = alloca [2 x i32], i32 1
  %156 = getelementptr [2 x i32], [2 x i32]* %155, i32 0, i32 0
  store i32 212, i32* %156
  %157 = getelementptr [2 x i32], [2 x i32]* %155, i32 0, i32 1
  store i32 213, i32* %157
  %158 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %159 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %158, [2 x i32]* %155)
  %160 = alloca [2 x i32], i32 1
  %161 = getelementptr [2 x i32], [2 x i32]* %160, i32 0, i32 0
  store i32 211, i32* %161
  %162 = getelementptr [2 x i32], [2 x i32]* %160, i32 0, i32 1
  store i32 8, i32* %162
  %163 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %164 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %163, [2 x i32]* %160)
  %165 = alloca [2 x i32], i32 1
  %166 = getelementptr [2 x i32], [2 x i32]* %165, i32 0, i32 0
  store i32 210, i32* %166
  %167 = getelementptr [2 x i32], [2 x i32]* %165, i32 0, i32 1
  store i32 8, i32* %167
  %168 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %169 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %168, [2 x i32]* %165)
  %170 = alloca [2 x i32], i32 1
  %171 = getelementptr [2 x i32], [2 x i32]* %170, i32 0, i32 0
  store i32 209, i32* %171
  %172 = getelementptr [2 x i32], [2 x i32]* %170, i32 0, i32 1
  store i32 8, i32* %172
  %173 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %174 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %173, [2 x i32]* %170)
  %175 = alloca [2 x i32], i32 1
  %176 = getelementptr [2 x i32], [2 x i32]* %175, i32 0, i32 0
  store i32 208, i32* %176
  %177 = getelementptr [2 x i32], [2 x i32]* %175, i32 0, i32 1
  store i32 8, i32* %177
  %178 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %179 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %178, [2 x i32]* %175)
  %180 = alloca [2 x i32], i32 1
  %181 = getelementptr [2 x i32], [2 x i32]* %180, i32 0, i32 0
  store i32 207, i32* %181
  %182 = getelementptr [2 x i32], [2 x i32]* %180, i32 0, i32 1
  store i32 122, i32* %182
  %183 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %184 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %183, [2 x i32]* %180)
  %185 = alloca [2 x i32], i32 1
  %186 = getelementptr [2 x i32], [2 x i32]* %185, i32 0, i32 0
  store i32 206, i32* %186
  %187 = getelementptr [2 x i32], [2 x i32]* %185, i32 0, i32 1
  store i32 122, i32* %187
  %188 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %189 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %188, [2 x i32]* %185)
  %190 = alloca [2 x i32], i32 1
  %191 = getelementptr [2 x i32], [2 x i32]* %190, i32 0, i32 0
  store i32 205, i32* %191
  %192 = getelementptr [2 x i32], [2 x i32]* %190, i32 0, i32 1
  store i32 122, i32* %192
  %193 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %194 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %193, [2 x i32]* %190)
  %195 = alloca [2 x i32], i32 1
  %196 = getelementptr [2 x i32], [2 x i32]* %195, i32 0, i32 0
  store i32 204, i32* %196
  %197 = getelementptr [2 x i32], [2 x i32]* %195, i32 0, i32 1
  store i32 128, i32* %197
  %198 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %199 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %198, [2 x i32]* %195)
  %200 = alloca [2 x i32], i32 1
  %201 = getelementptr [2 x i32], [2 x i32]* %200, i32 0, i32 0
  store i32 203, i32* %201
  %202 = getelementptr [2 x i32], [2 x i32]* %200, i32 0, i32 1
  store i32 47, i32* %202
  %203 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %204 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %203, [2 x i32]* %200)
  %205 = alloca [2 x i32], i32 1
  %206 = getelementptr [2 x i32], [2 x i32]* %205, i32 0, i32 0
  store i32 202, i32* %206
  %207 = getelementptr [2 x i32], [2 x i32]* %205, i32 0, i32 1
  store i32 47, i32* %207
  %208 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %209 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %208, [2 x i32]* %205)
  %210 = alloca [2 x i32], i32 1
  %211 = getelementptr [2 x i32], [2 x i32]* %210, i32 0, i32 0
  store i32 201, i32* %211
  %212 = getelementptr [2 x i32], [2 x i32]* %210, i32 0, i32 1
  store i32 47, i32* %212
  %213 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %214 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %213, [2 x i32]* %210)
  %215 = alloca [2 x i32], i32 1
  %216 = getelementptr [2 x i32], [2 x i32]* %215, i32 0, i32 0
  store i32 200, i32* %216
  %217 = getelementptr [2 x i32], [2 x i32]* %215, i32 0, i32 1
  store i32 47, i32* %217
  %218 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %219 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %218, [2 x i32]* %215)
  %220 = alloca [2 x i32], i32 1
  %221 = getelementptr [2 x i32], [2 x i32]* %220, i32 0, i32 0
  store i32 199, i32* %221
  %222 = getelementptr [2 x i32], [2 x i32]* %220, i32 0, i32 1
  store i32 47, i32* %222
  %223 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %224 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %223, [2 x i32]* %220)
  %225 = alloca [2 x i32], i32 1
  %226 = getelementptr [2 x i32], [2 x i32]* %225, i32 0, i32 0
  store i32 198, i32* %226
  %227 = getelementptr [2 x i32], [2 x i32]* %225, i32 0, i32 1
  store i32 47, i32* %227
  %228 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %229 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %228, [2 x i32]* %225)
  %230 = alloca [2 x i32], i32 1
  %231 = getelementptr [2 x i32], [2 x i32]* %230, i32 0, i32 0
  store i32 197, i32* %231
  %232 = getelementptr [2 x i32], [2 x i32]* %230, i32 0, i32 1
  store i32 47, i32* %232
  %233 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %234 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %233, [2 x i32]* %230)
  %235 = alloca [2 x i32], i32 1
  %236 = getelementptr [2 x i32], [2 x i32]* %235, i32 0, i32 0
  store i32 196, i32* %236
  %237 = getelementptr [2 x i32], [2 x i32]* %235, i32 0, i32 1
  store i32 47, i32* %237
  %238 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %239 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %238, [2 x i32]* %235)
  %240 = alloca [2 x i32], i32 1
  %241 = getelementptr [2 x i32], [2 x i32]* %240, i32 0, i32 0
  store i32 195, i32* %241
  %242 = getelementptr [2 x i32], [2 x i32]* %240, i32 0, i32 1
  store i32 47, i32* %242
  %243 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %244 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %243, [2 x i32]* %240)
  %245 = alloca [2 x i32], i32 1
  %246 = getelementptr [2 x i32], [2 x i32]* %245, i32 0, i32 0
  store i32 194, i32* %246
  %247 = getelementptr [2 x i32], [2 x i32]* %245, i32 0, i32 1
  store i32 47, i32* %247
  %248 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %249 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %248, [2 x i32]* %245)
  %250 = alloca [2 x i32], i32 1
  %251 = getelementptr [2 x i32], [2 x i32]* %250, i32 0, i32 0
  store i32 58, i32* %251
  %252 = getelementptr [2 x i32], [2 x i32]* %250, i32 0, i32 1
  store i32 59, i32* %252
  %253 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %254 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %253, [2 x i32]* %250)
  %255 = alloca [2 x i32], i32 1
  %256 = getelementptr [2 x i32], [2 x i32]* %255, i32 0, i32 0
  store i32 193, i32* %256
  %257 = getelementptr [2 x i32], [2 x i32]* %255, i32 0, i32 1
  store i32 62, i32* %257
  %258 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %259 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %258, [2 x i32]* %255)
  %260 = alloca [2 x i32], i32 1
  %261 = getelementptr [2 x i32], [2 x i32]* %260, i32 0, i32 0
  store i32 192, i32* %261
  %262 = getelementptr [2 x i32], [2 x i32]* %260, i32 0, i32 1
  store i32 62, i32* %262
  %263 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %264 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %263, [2 x i32]* %260)
  %265 = alloca [2 x i32], i32 1
  %266 = getelementptr [2 x i32], [2 x i32]* %265, i32 0, i32 0
  store i32 191, i32* %266
  %267 = getelementptr [2 x i32], [2 x i32]* %265, i32 0, i32 1
  store i32 62, i32* %267
  %268 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %269 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %268, [2 x i32]* %265)
  %270 = alloca [2 x i32], i32 1
  %271 = getelementptr [2 x i32], [2 x i32]* %270, i32 0, i32 0
  store i32 190, i32* %271
  %272 = getelementptr [2 x i32], [2 x i32]* %270, i32 0, i32 1
  store i32 62, i32* %272
  %273 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %274 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %273, [2 x i32]* %270)
  %275 = alloca [2 x i32], i32 1
  %276 = getelementptr [2 x i32], [2 x i32]* %275, i32 0, i32 0
  store i32 189, i32* %276
  %277 = getelementptr [2 x i32], [2 x i32]* %275, i32 0, i32 1
  store i32 62, i32* %277
  %278 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %279 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %278, [2 x i32]* %275)
  %280 = alloca [2 x i32], i32 1
  %281 = getelementptr [2 x i32], [2 x i32]* %280, i32 0, i32 0
  store i32 188, i32* %281
  %282 = getelementptr [2 x i32], [2 x i32]* %280, i32 0, i32 1
  store i32 80, i32* %282
  %283 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %284 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %283, [2 x i32]* %280)
  %285 = alloca [2 x i32], i32 1
  %286 = getelementptr [2 x i32], [2 x i32]* %285, i32 0, i32 0
  store i32 187, i32* %286
  %287 = getelementptr [2 x i32], [2 x i32]* %285, i32 0, i32 1
  store i32 80, i32* %287
  %288 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %289 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %288, [2 x i32]* %285)
  %290 = alloca [2 x i32], i32 1
  %291 = getelementptr [2 x i32], [2 x i32]* %290, i32 0, i32 0
  store i32 186, i32* %291
  %292 = getelementptr [2 x i32], [2 x i32]* %290, i32 0, i32 1
  store i32 80, i32* %292
  %293 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %294 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %293, [2 x i32]* %290)
  %295 = alloca [2 x i32], i32 1
  %296 = getelementptr [2 x i32], [2 x i32]* %295, i32 0, i32 0
  store i32 185, i32* %296
  %297 = getelementptr [2 x i32], [2 x i32]* %295, i32 0, i32 1
  store i32 80, i32* %297
  %298 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %299 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %298, [2 x i32]* %295)
  %300 = alloca [2 x i32], i32 1
  %301 = getelementptr [2 x i32], [2 x i32]* %300, i32 0, i32 0
  store i32 184, i32* %301
  %302 = getelementptr [2 x i32], [2 x i32]* %300, i32 0, i32 1
  store i32 80, i32* %302
  %303 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %304 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %303, [2 x i32]* %300)
  %305 = alloca [2 x i32], i32 1
  %306 = getelementptr [2 x i32], [2 x i32]* %305, i32 0, i32 0
  store i32 183, i32* %306
  %307 = getelementptr [2 x i32], [2 x i32]* %305, i32 0, i32 1
  store i32 110, i32* %307
  %308 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %309 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %308, [2 x i32]* %305)
  %310 = alloca [2 x i32], i32 1
  %311 = getelementptr [2 x i32], [2 x i32]* %310, i32 0, i32 0
  store i32 182, i32* %311
  %312 = getelementptr [2 x i32], [2 x i32]* %310, i32 0, i32 1
  store i32 110, i32* %312
  %313 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %314 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %313, [2 x i32]* %310)
  %315 = alloca [2 x i32], i32 1
  %316 = getelementptr [2 x i32], [2 x i32]* %315, i32 0, i32 0
  store i32 181, i32* %316
  %317 = getelementptr [2 x i32], [2 x i32]* %315, i32 0, i32 1
  store i32 110, i32* %317
  %318 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %319 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %318, [2 x i32]* %315)
  %320 = alloca [2 x i32], i32 1
  %321 = getelementptr [2 x i32], [2 x i32]* %320, i32 0, i32 0
  store i32 180, i32* %321
  %322 = getelementptr [2 x i32], [2 x i32]* %320, i32 0, i32 1
  store i32 74, i32* %322
  %323 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %324 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %323, [2 x i32]* %320)
  %325 = alloca [2 x i32], i32 1
  %326 = getelementptr [2 x i32], [2 x i32]* %325, i32 0, i32 0
  store i32 179, i32* %326
  %327 = getelementptr [2 x i32], [2 x i32]* %325, i32 0, i32 1
  store i32 74, i32* %327
  %328 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %329 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %328, [2 x i32]* %325)
  %330 = alloca [2 x i32], i32 1
  %331 = getelementptr [2 x i32], [2 x i32]* %330, i32 0, i32 0
  store i32 178, i32* %331
  %332 = getelementptr [2 x i32], [2 x i32]* %330, i32 0, i32 1
  store i32 131, i32* %332
  %333 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %334 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %333, [2 x i32]* %330)
  %335 = alloca [2 x i32], i32 1
  %336 = getelementptr [2 x i32], [2 x i32]* %335, i32 0, i32 0
  store i32 177, i32* %336
  %337 = getelementptr [2 x i32], [2 x i32]* %335, i32 0, i32 1
  store i32 131, i32* %337
  %338 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %339 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %338, [2 x i32]* %335)
  %340 = alloca [2 x i32], i32 1
  %341 = getelementptr [2 x i32], [2 x i32]* %340, i32 0, i32 0
  store i32 176, i32* %341
  %342 = getelementptr [2 x i32], [2 x i32]* %340, i32 0, i32 1
  store i32 131, i32* %342
  %343 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %344 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %343, [2 x i32]* %340)
  %345 = alloca [2 x i32], i32 1
  %346 = getelementptr [2 x i32], [2 x i32]* %345, i32 0, i32 0
  store i32 175, i32* %346
  %347 = getelementptr [2 x i32], [2 x i32]* %345, i32 0, i32 1
  store i32 131, i32* %347
  %348 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %349 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %348, [2 x i32]* %345)
  %350 = alloca [2 x i32], i32 1
  %351 = getelementptr [2 x i32], [2 x i32]* %350, i32 0, i32 0
  store i32 174, i32* %351
  %352 = getelementptr [2 x i32], [2 x i32]* %350, i32 0, i32 1
  store i32 131, i32* %352
  %353 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %354 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %353, [2 x i32]* %350)
  %355 = alloca [2 x i32], i32 1
  %356 = getelementptr [2 x i32], [2 x i32]* %355, i32 0, i32 0
  store i32 173, i32* %356
  %357 = getelementptr [2 x i32], [2 x i32]* %355, i32 0, i32 1
  store i32 131, i32* %357
  %358 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %359 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %358, [2 x i32]* %355)
  %360 = alloca [2 x i32], i32 1
  %361 = getelementptr [2 x i32], [2 x i32]* %360, i32 0, i32 0
  store i32 172, i32* %361
  %362 = getelementptr [2 x i32], [2 x i32]* %360, i32 0, i32 1
  store i32 131, i32* %362
  %363 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %364 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %363, [2 x i32]* %360)
  %365 = alloca [2 x i32], i32 1
  %366 = getelementptr [2 x i32], [2 x i32]* %365, i32 0, i32 0
  store i32 171, i32* %366
  %367 = getelementptr [2 x i32], [2 x i32]* %365, i32 0, i32 1
  store i32 131, i32* %367
  %368 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %369 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %368, [2 x i32]* %365)
  %370 = alloca [2 x i32], i32 1
  %371 = getelementptr [2 x i32], [2 x i32]* %370, i32 0, i32 0
  store i32 170, i32* %371
  %372 = getelementptr [2 x i32], [2 x i32]* %370, i32 0, i32 1
  store i32 131, i32* %372
  %373 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %374 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %373, [2 x i32]* %370)
  %375 = alloca [2 x i32], i32 1
  %376 = getelementptr [2 x i32], [2 x i32]* %375, i32 0, i32 0
  store i32 169, i32* %376
  %377 = getelementptr [2 x i32], [2 x i32]* %375, i32 0, i32 1
  store i32 131, i32* %377
  %378 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %379 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %378, [2 x i32]* %375)
  %380 = alloca [2 x i32], i32 1
  %381 = getelementptr [2 x i32], [2 x i32]* %380, i32 0, i32 0
  store i32 168, i32* %381
  %382 = getelementptr [2 x i32], [2 x i32]* %380, i32 0, i32 1
  store i32 83, i32* %382
  %383 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %384 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %383, [2 x i32]* %380)
  %385 = alloca [2 x i32], i32 1
  %386 = getelementptr [2 x i32], [2 x i32]* %385, i32 0, i32 0
  store i32 167, i32* %386
  %387 = getelementptr [2 x i32], [2 x i32]* %385, i32 0, i32 1
  store i32 83, i32* %387
  %388 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %389 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %388, [2 x i32]* %385)
  %390 = alloca [2 x i32], i32 1
  %391 = getelementptr [2 x i32], [2 x i32]* %390, i32 0, i32 0
  store i32 166, i32* %391
  %392 = getelementptr [2 x i32], [2 x i32]* %390, i32 0, i32 1
  store i32 83, i32* %392
  %393 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %394 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %393, [2 x i32]* %390)
  %395 = alloca [2 x i32], i32 1
  %396 = getelementptr [2 x i32], [2 x i32]* %395, i32 0, i32 0
  store i32 165, i32* %396
  %397 = getelementptr [2 x i32], [2 x i32]* %395, i32 0, i32 1
  store i32 83, i32* %397
  %398 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %399 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %398, [2 x i32]* %395)
  %400 = alloca [2 x i32], i32 1
  %401 = getelementptr [2 x i32], [2 x i32]* %400, i32 0, i32 0
  store i32 164, i32* %401
  %402 = getelementptr [2 x i32], [2 x i32]* %400, i32 0, i32 1
  store i32 83, i32* %402
  %403 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %404 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %403, [2 x i32]* %400)
  %405 = alloca [2 x i32], i32 1
  %406 = getelementptr [2 x i32], [2 x i32]* %405, i32 0, i32 0
  store i32 163, i32* %406
  %407 = getelementptr [2 x i32], [2 x i32]* %405, i32 0, i32 1
  store i32 83, i32* %407
  %408 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %409 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %408, [2 x i32]* %405)
  %410 = alloca [2 x i32], i32 1
  %411 = getelementptr [2 x i32], [2 x i32]* %410, i32 0, i32 0
  store i32 162, i32* %411
  %412 = getelementptr [2 x i32], [2 x i32]* %410, i32 0, i32 1
  store i32 83, i32* %412
  %413 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %414 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %413, [2 x i32]* %410)
  %415 = alloca [2 x i32], i32 1
  %416 = getelementptr [2 x i32], [2 x i32]* %415, i32 0, i32 0
  store i32 161, i32* %416
  %417 = getelementptr [2 x i32], [2 x i32]* %415, i32 0, i32 1
  store i32 95, i32* %417
  %418 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %419 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %418, [2 x i32]* %415)
  %420 = alloca [2 x i32], i32 1
  %421 = getelementptr [2 x i32], [2 x i32]* %420, i32 0, i32 0
  store i32 98, i32* %421
  %422 = getelementptr [2 x i32], [2 x i32]* %420, i32 0, i32 1
  store i32 95, i32* %422
  %423 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %424 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %423, [2 x i32]* %420)
  %425 = alloca [2 x i32], i32 1
  %426 = getelementptr [2 x i32], [2 x i32]* %425, i32 0, i32 0
  store i32 95, i32* %426
  %427 = getelementptr [2 x i32], [2 x i32]* %425, i32 0, i32 1
  store i32 95, i32* %427
  %428 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %429 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %428, [2 x i32]* %425)
  %430 = alloca [2 x i32], i32 1
  %431 = getelementptr [2 x i32], [2 x i32]* %430, i32 0, i32 0
  store i32 160, i32* %431
  %432 = getelementptr [2 x i32], [2 x i32]* %430, i32 0, i32 1
  store i32 95, i32* %432
  %433 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %434 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %433, [2 x i32]* %430)
  %435 = alloca [2 x i32], i32 1
  %436 = getelementptr [2 x i32], [2 x i32]* %435, i32 0, i32 0
  store i32 159, i32* %436
  %437 = getelementptr [2 x i32], [2 x i32]* %435, i32 0, i32 1
  store i32 95, i32* %437
  %438 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %439 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %438, [2 x i32]* %435)
  %440 = alloca [2 x i32], i32 1
  %441 = getelementptr [2 x i32], [2 x i32]* %440, i32 0, i32 0
  store i32 158, i32* %441
  %442 = getelementptr [2 x i32], [2 x i32]* %440, i32 0, i32 1
  store i32 95, i32* %442
  %443 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %444 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %443, [2 x i32]* %440)
  %445 = alloca [2 x i32], i32 1
  %446 = getelementptr [2 x i32], [2 x i32]* %445, i32 0, i32 0
  store i32 157, i32* %446
  %447 = getelementptr [2 x i32], [2 x i32]* %445, i32 0, i32 1
  store i32 95, i32* %447
  %448 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %449 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %448, [2 x i32]* %445)
  %450 = alloca [2 x i32], i32 1
  %451 = getelementptr [2 x i32], [2 x i32]* %450, i32 0, i32 0
  store i32 156, i32* %451
  %452 = getelementptr [2 x i32], [2 x i32]* %450, i32 0, i32 1
  store i32 77, i32* %452
  %453 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %454 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %453, [2 x i32]* %450)
  %455 = alloca [2 x i32], i32 1
  %456 = getelementptr [2 x i32], [2 x i32]* %455, i32 0, i32 0
  store i32 155, i32* %456
  %457 = getelementptr [2 x i32], [2 x i32]* %455, i32 0, i32 1
  store i32 77, i32* %457
  %458 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %459 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %458, [2 x i32]* %455)
  %460 = alloca [2 x i32], i32 1
  %461 = getelementptr [2 x i32], [2 x i32]* %460, i32 0, i32 0
  store i32 154, i32* %461
  %462 = getelementptr [2 x i32], [2 x i32]* %460, i32 0, i32 1
  store i32 77, i32* %462
  %463 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %464 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %463, [2 x i32]* %460)
  %465 = alloca [2 x i32], i32 1
  %466 = getelementptr [2 x i32], [2 x i32]* %465, i32 0, i32 0
  store i32 153, i32* %466
  %467 = getelementptr [2 x i32], [2 x i32]* %465, i32 0, i32 1
  store i32 77, i32* %467
  %468 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %469 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %468, [2 x i32]* %465)
  %470 = alloca [2 x i32], i32 1
  %471 = getelementptr [2 x i32], [2 x i32]* %470, i32 0, i32 0
  store i32 152, i32* %471
  %472 = getelementptr [2 x i32], [2 x i32]* %470, i32 0, i32 1
  store i32 77, i32* %472
  %473 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %474 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %473, [2 x i32]* %470)
  %475 = alloca [2 x i32], i32 1
  %476 = getelementptr [2 x i32], [2 x i32]* %475, i32 0, i32 0
  store i32 151, i32* %476
  %477 = getelementptr [2 x i32], [2 x i32]* %475, i32 0, i32 1
  store i32 77, i32* %477
  %478 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %479 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %478, [2 x i32]* %475)
  %480 = alloca [2 x i32], i32 1
  %481 = getelementptr [2 x i32], [2 x i32]* %480, i32 0, i32 0
  store i32 150, i32* %481
  %482 = getelementptr [2 x i32], [2 x i32]* %480, i32 0, i32 1
  store i32 77, i32* %482
  %483 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %484 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %483, [2 x i32]* %480)
  %485 = alloca [2 x i32], i32 1
  %486 = getelementptr [2 x i32], [2 x i32]* %485, i32 0, i32 0
  store i32 77, i32* %486
  %487 = getelementptr [2 x i32], [2 x i32]* %485, i32 0, i32 1
  store i32 77, i32* %487
  %488 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %489 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %488, [2 x i32]* %485)
  %490 = alloca [2 x i32], i32 1
  %491 = getelementptr [2 x i32], [2 x i32]* %490, i32 0, i32 0
  store i32 149, i32* %491
  %492 = getelementptr [2 x i32], [2 x i32]* %490, i32 0, i32 1
  store i32 77, i32* %492
  %493 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %494 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %493, [2 x i32]* %490)
  %495 = alloca [2 x i32], i32 1
  %496 = getelementptr [2 x i32], [2 x i32]* %495, i32 0, i32 0
  store i32 148, i32* %496
  %497 = getelementptr [2 x i32], [2 x i32]* %495, i32 0, i32 1
  store i32 77, i32* %497
  %498 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %499 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %498, [2 x i32]* %495)
  %500 = alloca [2 x i32], i32 1
  %501 = getelementptr [2 x i32], [2 x i32]* %500, i32 0, i32 0
  store i32 147, i32* %501
  %502 = getelementptr [2 x i32], [2 x i32]* %500, i32 0, i32 1
  store i32 77, i32* %502
  %503 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %504 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %503, [2 x i32]* %500)
  %505 = alloca [2 x i32], i32 1
  %506 = getelementptr [2 x i32], [2 x i32]* %505, i32 0, i32 0
  store i32 146, i32* %506
  %507 = getelementptr [2 x i32], [2 x i32]* %505, i32 0, i32 1
  store i32 77, i32* %507
  %508 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %509 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %508, [2 x i32]* %505)
  %510 = alloca [2 x i32], i32 1
  %511 = getelementptr [2 x i32], [2 x i32]* %510, i32 0, i32 0
  store i32 145, i32* %511
  %512 = getelementptr [2 x i32], [2 x i32]* %510, i32 0, i32 1
  store i32 77, i32* %512
  %513 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %514 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %513, [2 x i32]* %510)
  %515 = alloca [2 x i32], i32 1
  %516 = getelementptr [2 x i32], [2 x i32]* %515, i32 0, i32 0
  store i32 144, i32* %516
  %517 = getelementptr [2 x i32], [2 x i32]* %515, i32 0, i32 1
  store i32 77, i32* %517
  %518 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %519 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %518, [2 x i32]* %515)
  %520 = alloca [2 x i32], i32 1
  %521 = getelementptr [2 x i32], [2 x i32]* %520, i32 0, i32 0
  store i32 143, i32* %521
  %522 = getelementptr [2 x i32], [2 x i32]* %520, i32 0, i32 1
  store i32 140, i32* %522
  %523 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %524 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %523, [2 x i32]* %520)
  %525 = alloca [2 x i32], i32 1
  %526 = getelementptr [2 x i32], [2 x i32]* %525, i32 0, i32 0
  store i32 142, i32* %526
  %527 = getelementptr [2 x i32], [2 x i32]* %525, i32 0, i32 1
  store i32 140, i32* %527
  %528 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %529 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %528, [2 x i32]* %525)
  %530 = alloca [2 x i32], i32 1
  %531 = getelementptr [2 x i32], [2 x i32]* %530, i32 0, i32 0
  store i32 141, i32* %531
  %532 = getelementptr [2 x i32], [2 x i32]* %530, i32 0, i32 1
  store i32 140, i32* %532
  %533 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %534 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %533, [2 x i32]* %530)
  %535 = alloca [2 x i32], i32 1
  %536 = getelementptr [2 x i32], [2 x i32]* %535, i32 0, i32 0
  store i32 139, i32* %536
  %537 = getelementptr [2 x i32], [2 x i32]* %535, i32 0, i32 1
  store i32 140, i32* %537
  %538 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %539 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %538, [2 x i32]* %535)
  %540 = alloca [2 x i32], i32 1
  %541 = getelementptr [2 x i32], [2 x i32]* %540, i32 0, i32 0
  store i32 138, i32* %541
  %542 = getelementptr [2 x i32], [2 x i32]* %540, i32 0, i32 1
  store i32 29, i32* %542
  %543 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %544 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %543, [2 x i32]* %540)
  %545 = alloca [2 x i32], i32 1
  %546 = getelementptr [2 x i32], [2 x i32]* %545, i32 0, i32 0
  store i32 137, i32* %546
  %547 = getelementptr [2 x i32], [2 x i32]* %545, i32 0, i32 1
  store i32 5, i32* %547
  %548 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %549 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %548, [2 x i32]* %545)
  %550 = alloca [3 x i32], i32 1
  %551 = getelementptr [3 x i32], [3 x i32]* %550, i32 0, i32 0
  store i32 133, i32* %551
  %552 = getelementptr [3 x i32], [3 x i32]* %550, i32 0, i32 1
  store i32 134, i32* %552
  %553 = getelementptr [3 x i32], [3 x i32]* %550, i32 0, i32 2
  store i32 135, i32* %553
  %554 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %555 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %554, [3 x i32]* %550)
  %556 = alloca [3 x i32], i32 1
  %557 = getelementptr [3 x i32], [3 x i32]* %556, i32 0, i32 0
  store i32 130, i32* %557
  %558 = getelementptr [3 x i32], [3 x i32]* %556, i32 0, i32 1
  store i32 131, i32* %558
  %559 = getelementptr [3 x i32], [3 x i32]* %556, i32 0, i32 2
  store i32 132, i32* %559
  %560 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %561 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %560, [3 x i32]* %556)
  %562 = alloca [3 x i32], i32 1
  %563 = getelementptr [3 x i32], [3 x i32]* %562, i32 0, i32 0
  store i32 127, i32* %563
  %564 = getelementptr [3 x i32], [3 x i32]* %562, i32 0, i32 1
  store i32 128, i32* %564
  %565 = getelementptr [3 x i32], [3 x i32]* %562, i32 0, i32 2
  store i32 129, i32* %565
  %566 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %567 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %566, [3 x i32]* %562)
  %568 = alloca [3 x i32], i32 1
  %569 = getelementptr [3 x i32], [3 x i32]* %568, i32 0, i32 0
  store i32 124, i32* %569
  %570 = getelementptr [3 x i32], [3 x i32]* %568, i32 0, i32 1
  store i32 125, i32* %570
  %571 = getelementptr [3 x i32], [3 x i32]* %568, i32 0, i32 2
  store i32 126, i32* %571
  %572 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %573 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %572, [3 x i32]* %568)
  %574 = alloca [3 x i32], i32 1
  %575 = getelementptr [3 x i32], [3 x i32]* %574, i32 0, i32 0
  store i32 121, i32* %575
  %576 = getelementptr [3 x i32], [3 x i32]* %574, i32 0, i32 1
  store i32 122, i32* %576
  %577 = getelementptr [3 x i32], [3 x i32]* %574, i32 0, i32 2
  store i32 123, i32* %577
  %578 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %579 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %578, [3 x i32]* %574)
  %580 = alloca [3 x i32], i32 1
  %581 = getelementptr [3 x i32], [3 x i32]* %580, i32 0, i32 0
  store i32 118, i32* %581
  %582 = getelementptr [3 x i32], [3 x i32]* %580, i32 0, i32 1
  store i32 119, i32* %582
  %583 = getelementptr [3 x i32], [3 x i32]* %580, i32 0, i32 2
  store i32 120, i32* %583
  %584 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %585 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %584, [3 x i32]* %580)
  %586 = alloca [3 x i32], i32 1
  %587 = getelementptr [3 x i32], [3 x i32]* %586, i32 0, i32 0
  store i32 115, i32* %587
  %588 = getelementptr [3 x i32], [3 x i32]* %586, i32 0, i32 1
  store i32 116, i32* %588
  %589 = getelementptr [3 x i32], [3 x i32]* %586, i32 0, i32 2
  store i32 117, i32* %589
  %590 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %591 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %590, [3 x i32]* %586)
  %592 = alloca [3 x i32], i32 1
  %593 = getelementptr [3 x i32], [3 x i32]* %592, i32 0, i32 0
  store i32 112, i32* %593
  %594 = getelementptr [3 x i32], [3 x i32]* %592, i32 0, i32 1
  store i32 113, i32* %594
  %595 = getelementptr [3 x i32], [3 x i32]* %592, i32 0, i32 2
  store i32 114, i32* %595
  %596 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %597 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %596, [3 x i32]* %592)
  %598 = alloca [3 x i32], i32 1
  %599 = getelementptr [3 x i32], [3 x i32]* %598, i32 0, i32 0
  store i32 109, i32* %599
  %600 = getelementptr [3 x i32], [3 x i32]* %598, i32 0, i32 1
  store i32 110, i32* %600
  %601 = getelementptr [3 x i32], [3 x i32]* %598, i32 0, i32 2
  store i32 111, i32* %601
  %602 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %603 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %602, [3 x i32]* %598)
  %604 = alloca [3 x i32], i32 1
  %605 = getelementptr [3 x i32], [3 x i32]* %604, i32 0, i32 0
  store i32 106, i32* %605
  %606 = getelementptr [3 x i32], [3 x i32]* %604, i32 0, i32 1
  store i32 107, i32* %606
  %607 = getelementptr [3 x i32], [3 x i32]* %604, i32 0, i32 2
  store i32 108, i32* %607
  %608 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %609 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %608, [3 x i32]* %604)
  %610 = alloca [3 x i32], i32 1
  %611 = getelementptr [3 x i32], [3 x i32]* %610, i32 0, i32 0
  store i32 103, i32* %611
  %612 = getelementptr [3 x i32], [3 x i32]* %610, i32 0, i32 1
  store i32 104, i32* %612
  %613 = getelementptr [3 x i32], [3 x i32]* %610, i32 0, i32 2
  store i32 105, i32* %613
  %614 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %615 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %614, [3 x i32]* %610)
  %616 = alloca [3 x i32], i32 1
  %617 = getelementptr [3 x i32], [3 x i32]* %616, i32 0, i32 0
  store i32 100, i32* %617
  %618 = getelementptr [3 x i32], [3 x i32]* %616, i32 0, i32 1
  store i32 101, i32* %618
  %619 = getelementptr [3 x i32], [3 x i32]* %616, i32 0, i32 2
  store i32 102, i32* %619
  %620 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %621 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %620, [3 x i32]* %616)
  %622 = alloca [3 x i32], i32 1
  %623 = getelementptr [3 x i32], [3 x i32]* %622, i32 0, i32 0
  store i32 97, i32* %623
  %624 = getelementptr [3 x i32], [3 x i32]* %622, i32 0, i32 1
  store i32 98, i32* %624
  %625 = getelementptr [3 x i32], [3 x i32]* %622, i32 0, i32 2
  store i32 99, i32* %625
  %626 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %627 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %626, [3 x i32]* %622)
  %628 = alloca [3 x i32], i32 1
  %629 = getelementptr [3 x i32], [3 x i32]* %628, i32 0, i32 0
  store i32 94, i32* %629
  %630 = getelementptr [3 x i32], [3 x i32]* %628, i32 0, i32 1
  store i32 95, i32* %630
  %631 = getelementptr [3 x i32], [3 x i32]* %628, i32 0, i32 2
  store i32 96, i32* %631
  %632 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %633 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %632, [3 x i32]* %628)
  %634 = alloca [3 x i32], i32 1
  %635 = getelementptr [3 x i32], [3 x i32]* %634, i32 0, i32 0
  store i32 91, i32* %635
  %636 = getelementptr [3 x i32], [3 x i32]* %634, i32 0, i32 1
  store i32 92, i32* %636
  %637 = getelementptr [3 x i32], [3 x i32]* %634, i32 0, i32 2
  store i32 93, i32* %637
  %638 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %639 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %638, [3 x i32]* %634)
  %640 = alloca [3 x i32], i32 1
  %641 = getelementptr [3 x i32], [3 x i32]* %640, i32 0, i32 0
  store i32 88, i32* %641
  %642 = getelementptr [3 x i32], [3 x i32]* %640, i32 0, i32 1
  store i32 89, i32* %642
  %643 = getelementptr [3 x i32], [3 x i32]* %640, i32 0, i32 2
  store i32 90, i32* %643
  %644 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %645 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %644, [3 x i32]* %640)
  %646 = alloca [3 x i32], i32 1
  %647 = getelementptr [3 x i32], [3 x i32]* %646, i32 0, i32 0
  store i32 85, i32* %647
  %648 = getelementptr [3 x i32], [3 x i32]* %646, i32 0, i32 1
  store i32 86, i32* %648
  %649 = getelementptr [3 x i32], [3 x i32]* %646, i32 0, i32 2
  store i32 87, i32* %649
  %650 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %651 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %650, [3 x i32]* %646)
  %652 = alloca [3 x i32], i32 1
  %653 = getelementptr [3 x i32], [3 x i32]* %652, i32 0, i32 0
  store i32 82, i32* %653
  %654 = getelementptr [3 x i32], [3 x i32]* %652, i32 0, i32 1
  store i32 83, i32* %654
  %655 = getelementptr [3 x i32], [3 x i32]* %652, i32 0, i32 2
  store i32 84, i32* %655
  %656 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %657 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %656, [3 x i32]* %652)
  %658 = alloca [3 x i32], i32 1
  %659 = getelementptr [3 x i32], [3 x i32]* %658, i32 0, i32 0
  store i32 79, i32* %659
  %660 = getelementptr [3 x i32], [3 x i32]* %658, i32 0, i32 1
  store i32 80, i32* %660
  %661 = getelementptr [3 x i32], [3 x i32]* %658, i32 0, i32 2
  store i32 81, i32* %661
  %662 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %663 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %662, [3 x i32]* %658)
  %664 = alloca [3 x i32], i32 1
  %665 = getelementptr [3 x i32], [3 x i32]* %664, i32 0, i32 0
  store i32 76, i32* %665
  %666 = getelementptr [3 x i32], [3 x i32]* %664, i32 0, i32 1
  store i32 77, i32* %666
  %667 = getelementptr [3 x i32], [3 x i32]* %664, i32 0, i32 2
  store i32 78, i32* %667
  %668 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %669 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %668, [3 x i32]* %664)
  %670 = alloca [3 x i32], i32 1
  %671 = getelementptr [3 x i32], [3 x i32]* %670, i32 0, i32 0
  store i32 73, i32* %671
  %672 = getelementptr [3 x i32], [3 x i32]* %670, i32 0, i32 1
  store i32 74, i32* %672
  %673 = getelementptr [3 x i32], [3 x i32]* %670, i32 0, i32 2
  store i32 75, i32* %673
  %674 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %675 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %674, [3 x i32]* %670)
  %676 = alloca [3 x i32], i32 1
  %677 = getelementptr [3 x i32], [3 x i32]* %676, i32 0, i32 0
  store i32 70, i32* %677
  %678 = getelementptr [3 x i32], [3 x i32]* %676, i32 0, i32 1
  store i32 71, i32* %678
  %679 = getelementptr [3 x i32], [3 x i32]* %676, i32 0, i32 2
  store i32 72, i32* %679
  %680 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %681 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %680, [3 x i32]* %676)
  %682 = alloca [3 x i32], i32 1
  %683 = getelementptr [3 x i32], [3 x i32]* %682, i32 0, i32 0
  store i32 67, i32* %683
  %684 = getelementptr [3 x i32], [3 x i32]* %682, i32 0, i32 1
  store i32 68, i32* %684
  %685 = getelementptr [3 x i32], [3 x i32]* %682, i32 0, i32 2
  store i32 69, i32* %685
  %686 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %687 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %686, [3 x i32]* %682)
  %688 = alloca [3 x i32], i32 1
  %689 = getelementptr [3 x i32], [3 x i32]* %688, i32 0, i32 0
  store i32 64, i32* %689
  %690 = getelementptr [3 x i32], [3 x i32]* %688, i32 0, i32 1
  store i32 65, i32* %690
  %691 = getelementptr [3 x i32], [3 x i32]* %688, i32 0, i32 2
  store i32 66, i32* %691
  %692 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %693 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %692, [3 x i32]* %688)
  %694 = alloca [3 x i32], i32 1
  %695 = getelementptr [3 x i32], [3 x i32]* %694, i32 0, i32 0
  store i32 61, i32* %695
  %696 = getelementptr [3 x i32], [3 x i32]* %694, i32 0, i32 1
  store i32 62, i32* %696
  %697 = getelementptr [3 x i32], [3 x i32]* %694, i32 0, i32 2
  store i32 63, i32* %697
  %698 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %699 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %698, [3 x i32]* %694)
  %700 = alloca [3 x i32], i32 1
  %701 = getelementptr [3 x i32], [3 x i32]* %700, i32 0, i32 0
  store i32 58, i32* %701
  %702 = getelementptr [3 x i32], [3 x i32]* %700, i32 0, i32 1
  store i32 59, i32* %702
  %703 = getelementptr [3 x i32], [3 x i32]* %700, i32 0, i32 2
  store i32 60, i32* %703
  %704 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %705 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %704, [3 x i32]* %700)
  %706 = alloca [3 x i32], i32 1
  %707 = getelementptr [3 x i32], [3 x i32]* %706, i32 0, i32 0
  store i32 55, i32* %707
  %708 = getelementptr [3 x i32], [3 x i32]* %706, i32 0, i32 1
  store i32 56, i32* %708
  %709 = getelementptr [3 x i32], [3 x i32]* %706, i32 0, i32 2
  store i32 57, i32* %709
  %710 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %711 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %710, [3 x i32]* %706)
  %712 = alloca [3 x i32], i32 1
  %713 = getelementptr [3 x i32], [3 x i32]* %712, i32 0, i32 0
  store i32 52, i32* %713
  %714 = getelementptr [3 x i32], [3 x i32]* %712, i32 0, i32 1
  store i32 53, i32* %714
  %715 = getelementptr [3 x i32], [3 x i32]* %712, i32 0, i32 2
  store i32 54, i32* %715
  %716 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %717 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %716, [3 x i32]* %712)
  %718 = alloca [3 x i32], i32 1
  %719 = getelementptr [3 x i32], [3 x i32]* %718, i32 0, i32 0
  store i32 49, i32* %719
  %720 = getelementptr [3 x i32], [3 x i32]* %718, i32 0, i32 1
  store i32 50, i32* %720
  %721 = getelementptr [3 x i32], [3 x i32]* %718, i32 0, i32 2
  store i32 51, i32* %721
  %722 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %723 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %722, [3 x i32]* %718)
  %724 = alloca [3 x i32], i32 1
  %725 = getelementptr [3 x i32], [3 x i32]* %724, i32 0, i32 0
  store i32 46, i32* %725
  %726 = getelementptr [3 x i32], [3 x i32]* %724, i32 0, i32 1
  store i32 47, i32* %726
  %727 = getelementptr [3 x i32], [3 x i32]* %724, i32 0, i32 2
  store i32 48, i32* %727
  %728 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %729 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %728, [3 x i32]* %724)
  %730 = alloca [3 x i32], i32 1
  %731 = getelementptr [3 x i32], [3 x i32]* %730, i32 0, i32 0
  store i32 43, i32* %731
  %732 = getelementptr [3 x i32], [3 x i32]* %730, i32 0, i32 1
  store i32 44, i32* %732
  %733 = getelementptr [3 x i32], [3 x i32]* %730, i32 0, i32 2
  store i32 45, i32* %733
  %734 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %735 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %734, [3 x i32]* %730)
  %736 = alloca [3 x i32], i32 1
  %737 = getelementptr [3 x i32], [3 x i32]* %736, i32 0, i32 0
  store i32 40, i32* %737
  %738 = getelementptr [3 x i32], [3 x i32]* %736, i32 0, i32 1
  store i32 41, i32* %738
  %739 = getelementptr [3 x i32], [3 x i32]* %736, i32 0, i32 2
  store i32 42, i32* %739
  %740 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %741 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %740, [3 x i32]* %736)
  %742 = alloca [3 x i32], i32 1
  %743 = getelementptr [3 x i32], [3 x i32]* %742, i32 0, i32 0
  store i32 37, i32* %743
  %744 = getelementptr [3 x i32], [3 x i32]* %742, i32 0, i32 1
  store i32 38, i32* %744
  %745 = getelementptr [3 x i32], [3 x i32]* %742, i32 0, i32 2
  store i32 39, i32* %745
  %746 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %747 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %746, [3 x i32]* %742)
  %748 = alloca [3 x i32], i32 1
  %749 = getelementptr [3 x i32], [3 x i32]* %748, i32 0, i32 0
  store i32 34, i32* %749
  %750 = getelementptr [3 x i32], [3 x i32]* %748, i32 0, i32 1
  store i32 35, i32* %750
  %751 = getelementptr [3 x i32], [3 x i32]* %748, i32 0, i32 2
  store i32 36, i32* %751
  %752 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %753 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %752, [3 x i32]* %748)
  %754 = alloca [3 x i32], i32 1
  %755 = getelementptr [3 x i32], [3 x i32]* %754, i32 0, i32 0
  store i32 31, i32* %755
  %756 = getelementptr [3 x i32], [3 x i32]* %754, i32 0, i32 1
  store i32 32, i32* %756
  %757 = getelementptr [3 x i32], [3 x i32]* %754, i32 0, i32 2
  store i32 33, i32* %757
  %758 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %759 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %758, [3 x i32]* %754)
  %760 = alloca [3 x i32], i32 1
  %761 = getelementptr [3 x i32], [3 x i32]* %760, i32 0, i32 0
  store i32 28, i32* %761
  %762 = getelementptr [3 x i32], [3 x i32]* %760, i32 0, i32 1
  store i32 29, i32* %762
  %763 = getelementptr [3 x i32], [3 x i32]* %760, i32 0, i32 2
  store i32 30, i32* %763
  %764 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %765 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %764, [3 x i32]* %760)
  %766 = alloca [3 x i32], i32 1
  %767 = getelementptr [3 x i32], [3 x i32]* %766, i32 0, i32 0
  store i32 25, i32* %767
  %768 = getelementptr [3 x i32], [3 x i32]* %766, i32 0, i32 1
  store i32 26, i32* %768
  %769 = getelementptr [3 x i32], [3 x i32]* %766, i32 0, i32 2
  store i32 27, i32* %769
  %770 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %771 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %770, [3 x i32]* %766)
  %772 = alloca [3 x i32], i32 1
  %773 = getelementptr [3 x i32], [3 x i32]* %772, i32 0, i32 0
  store i32 22, i32* %773
  %774 = getelementptr [3 x i32], [3 x i32]* %772, i32 0, i32 1
  store i32 23, i32* %774
  %775 = getelementptr [3 x i32], [3 x i32]* %772, i32 0, i32 2
  store i32 24, i32* %775
  %776 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %777 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %776, [3 x i32]* %772)
  %778 = alloca [3 x i32], i32 1
  %779 = getelementptr [3 x i32], [3 x i32]* %778, i32 0, i32 0
  store i32 19, i32* %779
  %780 = getelementptr [3 x i32], [3 x i32]* %778, i32 0, i32 1
  store i32 20, i32* %780
  %781 = getelementptr [3 x i32], [3 x i32]* %778, i32 0, i32 2
  store i32 21, i32* %781
  %782 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %783 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %782, [3 x i32]* %778)
  %784 = alloca [3 x i32], i32 1
  %785 = getelementptr [3 x i32], [3 x i32]* %784, i32 0, i32 0
  store i32 16, i32* %785
  %786 = getelementptr [3 x i32], [3 x i32]* %784, i32 0, i32 1
  store i32 17, i32* %786
  %787 = getelementptr [3 x i32], [3 x i32]* %784, i32 0, i32 2
  store i32 18, i32* %787
  %788 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %789 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %788, [3 x i32]* %784)
  %790 = alloca [3 x i32], i32 1
  %791 = getelementptr [3 x i32], [3 x i32]* %790, i32 0, i32 0
  store i32 13, i32* %791
  %792 = getelementptr [3 x i32], [3 x i32]* %790, i32 0, i32 1
  store i32 14, i32* %792
  %793 = getelementptr [3 x i32], [3 x i32]* %790, i32 0, i32 2
  store i32 15, i32* %793
  %794 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %795 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %794, [3 x i32]* %790)
  %796 = alloca [3 x i32], i32 1
  %797 = getelementptr [3 x i32], [3 x i32]* %796, i32 0, i32 0
  store i32 10, i32* %797
  %798 = getelementptr [3 x i32], [3 x i32]* %796, i32 0, i32 1
  store i32 11, i32* %798
  %799 = getelementptr [3 x i32], [3 x i32]* %796, i32 0, i32 2
  store i32 12, i32* %799
  %800 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %801 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %800, [3 x i32]* %796)
  %802 = alloca [3 x i32], i32 1
  %803 = getelementptr [3 x i32], [3 x i32]* %802, i32 0, i32 0
  store i32 7, i32* %803
  %804 = getelementptr [3 x i32], [3 x i32]* %802, i32 0, i32 1
  store i32 8, i32* %804
  %805 = getelementptr [3 x i32], [3 x i32]* %802, i32 0, i32 2
  store i32 9, i32* %805
  %806 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %807 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %806, [3 x i32]* %802)
  %808 = alloca [3 x i32], i32 1
  %809 = getelementptr [3 x i32], [3 x i32]* %808, i32 0, i32 0
  store i32 4, i32* %809
  %810 = getelementptr [3 x i32], [3 x i32]* %808, i32 0, i32 1
  store i32 5, i32* %810
  %811 = getelementptr [3 x i32], [3 x i32]* %808, i32 0, i32 2
  store i32 6, i32* %811
  %812 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  %813 = call ccc i1 @eclair_btree_insert_value_0(%btree_t_0* %812, [3 x i32]* %808)
  %814 = alloca [3 x i32], i32 1
  %815 = alloca [3 x i32], i32 1
  %816 = getelementptr [3 x i32], [3 x i32]* %814, i32 0, i32 0
  store i32 0, i32* %816
  %817 = getelementptr [3 x i32], [3 x i32]* %814, i32 0, i32 1
  store i32 0, i32* %817
  %818 = getelementptr [3 x i32], [3 x i32]* %814, i32 0, i32 2
  store i32 0, i32* %818
  %819 = getelementptr [3 x i32], [3 x i32]* %815, i32 0, i32 0
  store i32 4294967295, i32* %819
  %820 = getelementptr [3 x i32], [3 x i32]* %815, i32 0, i32 1
  store i32 4294967295, i32* %820
  %821 = getelementptr [3 x i32], [3 x i32]* %815, i32 0, i32 2
  store i32 4294967295, i32* %821
  %822 = alloca %btree_iterator_t_0, i32 1
  %823 = alloca %btree_iterator_t_0, i32 1
  %824 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  call ccc void @eclair_btree_lower_bound_0(%btree_t_0* %824, [3 x i32]* %814, %btree_iterator_t_0* %822)
  %825 = getelementptr %program, %program* %arg_0, i32 0, i32 1
  call ccc void @eclair_btree_upper_bound_0(%btree_t_0* %825, [3 x i32]* %815, %btree_iterator_t_0* %823)
  br label %loop_0
loop_0:
  %826 = call ccc i1 @eclair_btree_iterator_is_equal_0(%btree_iterator_t_0* %822, %btree_iterator_t_0* %823)
  br i1 %826, label %if_0, label %end_if_0
if_0:
  br label %range_query.end
end_if_0:
  %827 = call ccc [3 x i32]* @eclair_btree_iterator_current_0(%btree_iterator_t_0* %822)
  %828 = alloca [2 x i32], i32 1
  %829 = getelementptr [2 x i32], [2 x i32]* %828, i32 0, i32 0
  %830 = getelementptr [3 x i32], [3 x i32]* %827, i32 0, i32 1
  %831 = load i32, i32* %830
  store i32 %831, i32* %829
  %832 = getelementptr [2 x i32], [2 x i32]* %828, i32 0, i32 1
  %833 = getelementptr [3 x i32], [3 x i32]* %827, i32 0, i32 1
  %834 = load i32, i32* %833
  store i32 %834, i32* %832
  %835 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  %836 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %835, [2 x i32]* %828)
  call ccc void @eclair_btree_iterator_next_0(%btree_iterator_t_0* %822)
  br label %loop_0
range_query.end:
  %837 = alloca [1 x i32], i32 1
  %838 = alloca [1 x i32], i32 1
  %839 = getelementptr [1 x i32], [1 x i32]* %837, i32 0, i32 0
  store i32 0, i32* %839
  %840 = getelementptr [1 x i32], [1 x i32]* %838, i32 0, i32 0
  store i32 4294967295, i32* %840
  %841 = alloca %btree_iterator_t_2, i32 1
  %842 = alloca %btree_iterator_t_2, i32 1
  %843 = getelementptr %program, %program* %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_lower_bound_2(%btree_t_2* %843, [1 x i32]* %837, %btree_iterator_t_2* %841)
  %844 = getelementptr %program, %program* %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_upper_bound_2(%btree_t_2* %844, [1 x i32]* %838, %btree_iterator_t_2* %842)
  br label %loop_1
loop_1:
  %845 = call ccc i1 @eclair_btree_iterator_is_equal_2(%btree_iterator_t_2* %841, %btree_iterator_t_2* %842)
  br i1 %845, label %if_1, label %end_if_1
if_1:
  br label %range_query.end_1
end_if_1:
  %846 = call ccc [1 x i32]* @eclair_btree_iterator_current_2(%btree_iterator_t_2* %841)
  %847 = alloca [2 x i32], i32 1
  %848 = getelementptr [2 x i32], [2 x i32]* %847, i32 0, i32 0
  %849 = getelementptr [1 x i32], [1 x i32]* %846, i32 0, i32 0
  %850 = load i32, i32* %849
  store i32 %850, i32* %848
  %851 = getelementptr [2 x i32], [2 x i32]* %847, i32 0, i32 1
  store i32 0, i32* %851
  %852 = alloca [2 x i32], i32 1
  %853 = getelementptr [2 x i32], [2 x i32]* %852, i32 0, i32 0
  %854 = getelementptr [1 x i32], [1 x i32]* %846, i32 0, i32 0
  %855 = load i32, i32* %854
  store i32 %855, i32* %853
  %856 = getelementptr [2 x i32], [2 x i32]* %852, i32 0, i32 1
  store i32 4294967295, i32* %856
  %857 = alloca %btree_iterator_t_1, i32 1
  %858 = alloca %btree_iterator_t_1, i32 1
  %859 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_1(%btree_t_1* %859, [2 x i32]* %847, %btree_iterator_t_1* %857)
  %860 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_1(%btree_t_1* %860, [2 x i32]* %852, %btree_iterator_t_1* %858)
  %861 = call ccc i1 @eclair_btree_iterator_is_equal_1(%btree_iterator_t_1* %857, %btree_iterator_t_1* %858)
  br i1 %861, label %if_2, label %end_if_2
if_2:
  %862 = alloca [1 x i32], i32 1
  %863 = getelementptr [1 x i32], [1 x i32]* %862, i32 0, i32 0
  %864 = getelementptr [1 x i32], [1 x i32]* %846, i32 0, i32 0
  %865 = load i32, i32* %864
  store i32 %865, i32* %863
  %866 = getelementptr %program, %program* %arg_0, i32 0, i32 3
  %867 = call ccc i1 @eclair_btree_insert_value_2(%btree_t_2* %866, [1 x i32]* %862)
  br label %end_if_2
end_if_2:
  call ccc void @eclair_btree_iterator_next_2(%btree_iterator_t_2* %841)
  br label %loop_1
range_query.end_1:
  %868 = alloca [1 x i32], i32 1
  %869 = alloca [1 x i32], i32 1
  %870 = getelementptr [1 x i32], [1 x i32]* %868, i32 0, i32 0
  store i32 0, i32* %870
  %871 = getelementptr [1 x i32], [1 x i32]* %869, i32 0, i32 0
  store i32 4294967295, i32* %871
  %872 = alloca %btree_iterator_t_2, i32 1
  %873 = alloca %btree_iterator_t_2, i32 1
  %874 = getelementptr %program, %program* %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_lower_bound_2(%btree_t_2* %874, [1 x i32]* %868, %btree_iterator_t_2* %872)
  %875 = getelementptr %program, %program* %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_upper_bound_2(%btree_t_2* %875, [1 x i32]* %869, %btree_iterator_t_2* %873)
  br label %loop_2
loop_2:
  %876 = call ccc i1 @eclair_btree_iterator_is_equal_2(%btree_iterator_t_2* %872, %btree_iterator_t_2* %873)
  br i1 %876, label %if_3, label %end_if_3
if_3:
  br label %range_query.end_2
end_if_3:
  %877 = call ccc [1 x i32]* @eclair_btree_iterator_current_2(%btree_iterator_t_2* %872)
  %878 = alloca [2 x i32], i32 1
  %879 = alloca [2 x i32], i32 1
  %880 = getelementptr [2 x i32], [2 x i32]* %878, i32 0, i32 0
  %881 = getelementptr [1 x i32], [1 x i32]* %877, i32 0, i32 0
  %882 = load i32, i32* %881
  store i32 %882, i32* %880
  %883 = getelementptr [2 x i32], [2 x i32]* %878, i32 0, i32 1
  store i32 0, i32* %883
  %884 = getelementptr [2 x i32], [2 x i32]* %879, i32 0, i32 0
  %885 = getelementptr [1 x i32], [1 x i32]* %877, i32 0, i32 0
  %886 = load i32, i32* %885
  store i32 %886, i32* %884
  %887 = getelementptr [2 x i32], [2 x i32]* %879, i32 0, i32 1
  store i32 4294967295, i32* %887
  %888 = alloca %btree_iterator_t_1, i32 1
  %889 = alloca %btree_iterator_t_1, i32 1
  %890 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_1(%btree_t_1* %890, [2 x i32]* %878, %btree_iterator_t_1* %888)
  %891 = getelementptr %program, %program* %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_1(%btree_t_1* %891, [2 x i32]* %879, %btree_iterator_t_1* %889)
  br label %loop_3
loop_3:
  %892 = call ccc i1 @eclair_btree_iterator_is_equal_1(%btree_iterator_t_1* %888, %btree_iterator_t_1* %889)
  br i1 %892, label %if_4, label %end_if_4
if_4:
  br label %range_query.end_3
end_if_4:
  %893 = call ccc [2 x i32]* @eclair_btree_iterator_current_1(%btree_iterator_t_1* %888)
  %894 = alloca [1 x i32], i32 1
  %895 = getelementptr [1 x i32], [1 x i32]* %894, i32 0, i32 0
  %896 = getelementptr [2 x i32], [2 x i32]* %893, i32 0, i32 1
  %897 = load i32, i32* %896
  store i32 %897, i32* %895
  %898 = getelementptr %program, %program* %arg_0, i32 0, i32 4
  %899 = call ccc i1 @eclair_btree_insert_value_2(%btree_t_2* %898, [1 x i32]* %894)
  call ccc void @eclair_btree_iterator_next_1(%btree_iterator_t_1* %888)
  br label %loop_3
range_query.end_3:
  call ccc void @eclair_btree_iterator_next_2(%btree_iterator_t_2* %872)
  br label %loop_2
range_query.end_2:
  ret void
}

define external ccc void @eclair_add_facts(%program* %eclair_program_0, i32 %fact_type_0, i32* %memory_0, i32 %fact_count_0) "wasm-export-name"="eclair_add_facts" {
start:
  switch i32 %fact_type_0, label %switch.default_0 [i32 136, label %normalise_category_0 i32 0, label %user_package_category_0]
normalise_category_0:
  %0 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 2
  %1 = bitcast i32* %memory_0 to [2 x i32]*
  br label %for_begin_0
for_begin_0:
  %2 = phi i32 [0, %normalise_category_0], [%6, %for_body_0]
  %3 = icmp ult i32 %2, %fact_count_0
  br i1 %3, label %for_body_0, label %for_end_0
for_body_0:
  %4 = getelementptr [2 x i32], [2 x i32]* %1, i32 %2
  %5 = call ccc i1 @eclair_btree_insert_value_1(%btree_t_1* %0, [2 x i32]* %4)
  %6 = add i32 1, %2
  br label %for_begin_0
for_end_0:
  ret void
user_package_category_0:
  %7 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 5
  %8 = bitcast i32* %memory_0 to [1 x i32]*
  br label %for_begin_1
for_begin_1:
  %9 = phi i32 [0, %user_package_category_0], [%13, %for_body_1]
  %10 = icmp ult i32 %9, %fact_count_0
  br i1 %10, label %for_body_1, label %for_end_1
for_body_1:
  %11 = getelementptr [1 x i32], [1 x i32]* %8, i32 %9
  %12 = call ccc i1 @eclair_btree_insert_value_2(%btree_t_2* %7, [1 x i32]* %11)
  %13 = add i32 1, %9
  br label %for_begin_1
for_end_1:
  ret void
switch.default_0:
  ret void
}

define external ccc void @eclair_add_fact(%program* %eclair_program_0, i32 %fact_type_0, i32* %memory_0) "wasm-export-name"="eclair_add_fact" {
start:
  call ccc void @eclair_add_facts(%program* %eclair_program_0, i32 %fact_type_0, i32* %memory_0, i32 1)
  ret void
}

define external ccc i32* @eclair_get_facts(%program* %eclair_program_0, i32 %fact_type_0) "wasm-export-name"="eclair_get_facts" {
start:
  switch i32 %fact_type_0, label %switch.default_0 [i32 3, label %flora_category_0 i32 2, label %normalise_issue_0 i32 1, label %normalised_package_category_0]
flora_category_0:
  %0 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 1
  %1 = call ccc i64 @eclair_btree_size_0(%btree_t_0* %0)
  %2 = trunc i64 %1 to i32
  %3 = mul i32 %2, 12
  %4 = call ccc i8* @malloc(i32 %3)
  %5 = bitcast i8* %4 to [3 x i32]*
  %6 = alloca i32, i32 1
  store i32 0, i32* %6
  %7 = alloca %btree_iterator_t_0, i32 1
  %8 = alloca %btree_iterator_t_0, i32 1
  call ccc void @eclair_btree_begin_0(%btree_t_0* %0, %btree_iterator_t_0* %7)
  call ccc void @eclair_btree_end_0(%btree_t_0* %0, %btree_iterator_t_0* %8)
  br label %while_begin_0
while_begin_0:
  %9 = call ccc i1 @eclair_btree_iterator_is_equal_0(%btree_iterator_t_0* %7, %btree_iterator_t_0* %8)
  %10 = select i1 %9, i1 0, i1 1
  br i1 %10, label %while_body_0, label %while_end_0
while_body_0:
  %11 = load i32, i32* %6
  %12 = getelementptr [3 x i32], [3 x i32]* %5, i32 %11
  %13 = call ccc [3 x i32]* @eclair_btree_iterator_current_0(%btree_iterator_t_0* %7)
  %14 = getelementptr [3 x i32], [3 x i32]* %13, i32 0
  %15 = load [3 x i32], [3 x i32]* %14
  %16 = getelementptr [3 x i32], [3 x i32]* %12, i32 0
  store [3 x i32] %15, [3 x i32]* %16
  %17 = add i32 %11, 1
  store i32 %17, i32* %6
  call ccc void @eclair_btree_iterator_next_0(%btree_iterator_t_0* %7)
  br label %while_begin_0
while_end_0:
  %18 = bitcast i8* %4 to i32*
  ret i32* %18
normalise_issue_0:
  %19 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 3
  %20 = call ccc i64 @eclair_btree_size_2(%btree_t_2* %19)
  %21 = trunc i64 %20 to i32
  %22 = mul i32 %21, 4
  %23 = call ccc i8* @malloc(i32 %22)
  %24 = bitcast i8* %23 to [1 x i32]*
  %25 = alloca i32, i32 1
  store i32 0, i32* %25
  %26 = alloca %btree_iterator_t_2, i32 1
  %27 = alloca %btree_iterator_t_2, i32 1
  call ccc void @eclair_btree_begin_2(%btree_t_2* %19, %btree_iterator_t_2* %26)
  call ccc void @eclair_btree_end_2(%btree_t_2* %19, %btree_iterator_t_2* %27)
  br label %while_begin_1
while_begin_1:
  %28 = call ccc i1 @eclair_btree_iterator_is_equal_2(%btree_iterator_t_2* %26, %btree_iterator_t_2* %27)
  %29 = select i1 %28, i1 0, i1 1
  br i1 %29, label %while_body_1, label %while_end_1
while_body_1:
  %30 = load i32, i32* %25
  %31 = getelementptr [1 x i32], [1 x i32]* %24, i32 %30
  %32 = call ccc [1 x i32]* @eclair_btree_iterator_current_2(%btree_iterator_t_2* %26)
  %33 = getelementptr [1 x i32], [1 x i32]* %32, i32 0
  %34 = load [1 x i32], [1 x i32]* %33
  %35 = getelementptr [1 x i32], [1 x i32]* %31, i32 0
  store [1 x i32] %34, [1 x i32]* %35
  %36 = add i32 %30, 1
  store i32 %36, i32* %25
  call ccc void @eclair_btree_iterator_next_2(%btree_iterator_t_2* %26)
  br label %while_begin_1
while_end_1:
  %37 = bitcast i8* %23 to i32*
  ret i32* %37
normalised_package_category_0:
  %38 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 4
  %39 = call ccc i64 @eclair_btree_size_2(%btree_t_2* %38)
  %40 = trunc i64 %39 to i32
  %41 = mul i32 %40, 4
  %42 = call ccc i8* @malloc(i32 %41)
  %43 = bitcast i8* %42 to [1 x i32]*
  %44 = alloca i32, i32 1
  store i32 0, i32* %44
  %45 = alloca %btree_iterator_t_2, i32 1
  %46 = alloca %btree_iterator_t_2, i32 1
  call ccc void @eclair_btree_begin_2(%btree_t_2* %38, %btree_iterator_t_2* %45)
  call ccc void @eclair_btree_end_2(%btree_t_2* %38, %btree_iterator_t_2* %46)
  br label %while_begin_2
while_begin_2:
  %47 = call ccc i1 @eclair_btree_iterator_is_equal_2(%btree_iterator_t_2* %45, %btree_iterator_t_2* %46)
  %48 = select i1 %47, i1 0, i1 1
  br i1 %48, label %while_body_2, label %while_end_2
while_body_2:
  %49 = load i32, i32* %44
  %50 = getelementptr [1 x i32], [1 x i32]* %43, i32 %49
  %51 = call ccc [1 x i32]* @eclair_btree_iterator_current_2(%btree_iterator_t_2* %45)
  %52 = getelementptr [1 x i32], [1 x i32]* %51, i32 0
  %53 = load [1 x i32], [1 x i32]* %52
  %54 = getelementptr [1 x i32], [1 x i32]* %50, i32 0
  store [1 x i32] %53, [1 x i32]* %54
  %55 = add i32 %49, 1
  store i32 %55, i32* %44
  call ccc void @eclair_btree_iterator_next_2(%btree_iterator_t_2* %45)
  br label %while_begin_2
while_end_2:
  %56 = bitcast i8* %42 to i32*
  ret i32* %56
switch.default_0:
  ret i32* zeroinitializer
}

define external ccc void @eclair_free_buffer(i32* %buffer_0) "wasm-export-name"="eclair_free_buffer" {
start:
  %0 = bitcast i32* %buffer_0 to i8*
  call ccc void @free(i8* %0)
  ret void
}

define external ccc i32 @eclair_fact_count(%program* %eclair_program_0, i32 %fact_type_0) "wasm-export-name"="eclair_fact_count" {
start:
  switch i32 %fact_type_0, label %switch.default_0 [i32 3, label %flora_category_0 i32 2, label %normalise_issue_0 i32 1, label %normalised_package_category_0]
flora_category_0:
  %0 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 1
  %1 = call ccc i64 @eclair_btree_size_0(%btree_t_0* %0)
  %2 = trunc i64 %1 to i32
  ret i32 %2
normalise_issue_0:
  %3 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 3
  %4 = call ccc i64 @eclair_btree_size_2(%btree_t_2* %3)
  %5 = trunc i64 %4 to i32
  ret i32 %5
normalised_package_category_0:
  %6 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 4
  %7 = call ccc i64 @eclair_btree_size_2(%btree_t_2* %6)
  %8 = trunc i64 %7 to i32
  ret i32 %8
switch.default_0:
  ret i32 0
}

define external ccc i32 @eclair_encode_string(%program* %eclair_program_0, i32 %string_length_0, i8* %string_data_0) "wasm-export-name"="eclair_encode_string" {
start:
  %0 = call ccc i8* @malloc(i32 %string_length_0)
  %1 = zext i32 %string_length_0 to i64
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %0, i8* %string_data_0, i64 %1, i1 0)
  %2 = alloca %symbol_t, i32 1
  call ccc void @eclair_symbol_init(%symbol_t* %2, i32 %string_length_0, i8* %0)
  %3 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 0
  %4 = call ccc i32 @eclair_symbol_table_lookup_index(%symbol_table* %3, %symbol_t* %2)
  %5 = icmp ne i32 %4, 4294967295
  br i1 %5, label %if_0, label %end_if_0
if_0:
  call ccc void @free(i8* %0)
  ret i32 %4
end_if_0:
  %6 = call ccc i32 @eclair_symbol_table_find_or_insert(%symbol_table* %3, %symbol_t* %2)
  ret i32 %6
}

define external ccc i8* @eclair_decode_string(%program* %eclair_program_0, i32 %string_index_0) "wasm-export-name"="eclair_decode_string" {
start:
  %0 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 0
  %1 = call ccc i1 @eclair_symbol_table_contains_index(%symbol_table* %0, i32 %string_index_0)
  br i1 %1, label %if_0, label %end_if_0
if_0:
  %2 = call ccc %symbol_t* @eclair_symbol_table_lookup_symbol(%symbol_table* %0, i32 %string_index_0)
  %3 = bitcast %symbol_t* %2 to i8*
  ret i8* %3
end_if_0:
  ret i8* zeroinitializer
}
