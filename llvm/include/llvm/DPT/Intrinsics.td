// PA primitives
def int_pa_tagc : Intrinsic<[ llvm_anyptr_ty ], [ llvm_ptr_ty, llvm_i64_ty ], [IntrNoMem]>;
def int_pa_tagd : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_ptr_ty ], [IntrNoMem]>;
def int_pa_tagd_mod : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_i64_ty ], [IntrNoMem]>;
def int_pa_autc : Intrinsic<[ llvm_anyptr_ty ], [ llvm_ptr_ty, llvm_i64_ty ], [IntrNoMem]>;
def int_pa_autd : Intrinsic<[ llvm_anyptr_ty ], [ llvm_ptr_ty, llvm_i64_ty ], [IntrNoMem]>;
def int_pa_xtag : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty ], [IntrNoMem]>;

// DPT primitives
def int_dpt_cstr : Intrinsic<[], [ llvm_ptr_ty, llvm_i64_ty ]>;
def int_dpt_cclr : Intrinsic<[], [ llvm_ptr_ty ]>;

// Bitmap Manipulation primitives
def int_bm_bsetm : Intrinsic<[ ], [ llvm_ptr_ty ]>;
def int_bm_bclrm : Intrinsic<[ ], [ llvm_ptr_ty ]>;
def int_bm_bextm : Intrinsic<[ llvm_anyint_ty ], [ llvm_ptr_ty ]>;

//def int_dpt_csrch : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_i64_ty ]>;
//def int_dpt_csrrc : Intrinsic<[llvm_anyptr_ty], [ llvm_i64_ty ]>;
//def int_dpt_csrrs : Intrinsic<[llvm_anyptr_ty], [ llvm_i64_ty ]>;
//def int_dpt_bndstr : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_i64_ty ]>;
//def int_dpt_bndclr : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty ]>;
//def int_dpt_bndsrch : Intrinsic<[llvm_any_ty], [ llvm_ptr_ty, llvm_i64_ty ]>;

//def int_aos_alloc : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_i64_ty ], [IntrNoMem]>;
//def int_aos_dealloc : Intrinsic<[], [ llvm_ptr_ty ]>;
//def int_aos_element : Intrinsic<[], [ llvm_ptr_ty, llvm_i64_ty, llvm_i64_ty ]>;
//def int_aos_element_ret : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_i64_ty, llvm_i64_ty ]>;
////def int_aos_element : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_i64_ty, llvm_i64_ty ]>;
//def int_aos_sign : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_i64_ty, llvm_i64_ty ]>;
////def int_aos_dealloc : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty ], [IntrNoMem]>;
//def int_aos_malloc : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_i64_ty ], [IntrNoMem]>;
//def int_aos_calloc : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_i64_ty ], [IntrNoMem]>;
////def int_aos_realloc : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_i64_ty ], [IntrNoMem]>;
//def int_aos_realloc : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty, llvm_ptr_ty, llvm_i64_ty ], [IntrNoMem]>;
//def int_aos_free : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty ], [IntrNoMem]>;
////def int_aos_free : Intrinsic<[], [ llvm_ptr_ty ], [IntrNoMem]>;
//def int_aos_autm : Intrinsic<[llvm_anyptr_ty], [ LLVMMatchType<0> ], [IntrNoMem]>;
////def int_aos_autm : Intrinsic<[llvm_anyptr_ty], [ LLVMMatchType<0> ], [IntrNoMem]>;
////def int_aos_autm : Intrinsic<[llvm_anyptr_ty], [ llvm_ptr_ty ], [IntrNoMem]>;
