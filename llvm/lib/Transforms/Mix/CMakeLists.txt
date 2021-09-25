add_llvm_component_library(LLVMMix
  Mix.cpp
  MixContext.cpp
  Types.cpp

  DEPENDS
  intrinsics_gen

  LINK_COMPONENTS
  Analysis
  Core
  IPO
  Support
  )
