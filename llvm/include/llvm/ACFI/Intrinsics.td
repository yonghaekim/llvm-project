// PA primitives
//def int_pa_tagc : Intrinsic<[ llvm_anyptr_ty ], [ llvm_ptr_ty, llvm_i64_ty ], [IntrNoMem]>;
//def int_pa_autc : Intrinsic<[ llvm_anyptr_ty ], [ llvm_ptr_ty, llvm_i64_ty ], [IntrNoMem]>;
//def int_pa_xtag : Intrinsic<[ llvm_anyptr_ty ], [ llvm_ptr_ty ], [IntrNoMem]>;

// ACFI primitives
def int_acfi_estr : Intrinsic<[], [ llvm_ptr_ty, llvm_i64_ty ]>;
def int_acfi_eclr : Intrinsic<[], [ llvm_ptr_ty ]>;
def int_acfi_eact : Intrinsic<[], [ llvm_ptr_ty, llvm_i64_ty ]>;
def int_acfi_edea : Intrinsic<[], [ llvm_ptr_ty, llvm_i64_ty ]>;
def int_acfi_echk : Intrinsic<[], [ llvm_ptr_ty, llvm_i64_ty ]>;

//// WPB (Write-Protected Bitmap) primitives
//def int_bm_bsetm : Intrinsic<[ ], [ llvm_ptr_ty ]>;
//def int_bm_bclrm : Intrinsic<[ ], [ llvm_ptr_ty ]>;
//def int_bm_bextm : Intrinsic<[ llvm_anyptr_ty ], [ llvm_ptr_ty ]>;

