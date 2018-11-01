add_llvm_library(LLVMMix
  Mix.cpp
  MixContext.cpp
  Types.cpp

  DEPENDS
  intrinsics_gen
  )
