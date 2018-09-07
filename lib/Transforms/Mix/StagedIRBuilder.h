//===- StagedIRBuilder.h ----------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the helper class used to build LLVM IR at a later
// execution stage.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TRANSFORMS_MIX_STAGEDIRBUILDER_H
#define LLVM_LIB_TRANSFORMS_MIX_STAGEDIRBUILDER_H

#include "Types.h"

#include "llvm-c/Core.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"

#include <cassert>
#include <functional>
#include <memory>
#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>

namespace llvm {

namespace mix {

// Staged global state.
class StagedModule {
public:
  Value *Context{};
  Instruction *Builder{};
  Instruction *Module{};

  ~StagedModule() { assert(!Parent && "StagedModule is not disposed"); }

  template <typename IRBuilder>
  static StagedModule build(IRBuilder &B, Value *StagedContext,
                            const Twine &ModuleID);

  template <typename IRBuilder> void dispose(IRBuilder &B);

  Function *getParent() const { return Parent; }

  Instruction *lookupType(Type *Ty) const { return Types.lookup(Ty); }

  void setStagedType(Type *Ty, Instruction *StagedTy) {
    bool Inserted;
    std::tie(std::ignore, Inserted) = Types.try_emplace(Ty, StagedTy);
    assert(Inserted && "The type is already staged");
    (void)Inserted;
  }

  Type *getDoubleTy() { return mix::getDoubleTy(Parent->getContext()); }
  IntegerType *getBoolTy() { return mix::getBoolTy(Parent->getContext()); }
  IntegerType *getIntPredicateTy() {
    return mix::getIntPredicateTy(Parent->getContext());
  }
  IntegerType *getLinkageTy() {
    return mix::getLinkageTy(Parent->getContext());
  }
  IntegerType *getOpcodeTy() { return mix::getOpcodeTy(Parent->getContext()); }
  IntegerType *getUnsignedIntTy() {
    return mix::getUnsignedIntTy(Parent->getContext());
  }
  IntegerType *getUnsignedLongLongIntTy() {
    return mix::getUnsignedLongLongIntTy(Parent->getContext());
  }
  PointerType *getBasicBlockPtrTy() {
    return mix::getBasicBlockPtrTy(Parent->getContext());
  }
  PointerType *getBuilderPtrTy() {
    return mix::getBuilderPtrTy(Parent->getContext());
  }
  PointerType *getCharPtrTy() {
    return mix::getCharPtrTy(Parent->getContext());
  }
  PointerType *getContextPtrTy() {
    return mix::getContextPtrTy(Parent->getContext());
  }
  PointerType *getTypePtrTy() {
    return mix::getTypePtrTy(Parent->getContext());
  }
  PointerType *getValuePtrTy() {
    return mix::getValuePtrTy(Parent->getContext());
  }

  Constant *getAPIFunction(const Twine &Name, Type *Result,
                           ArrayRef<Type *> Params) {
    return Parent->getParent()->getOrInsertFunction(
        Name.str(), FunctionType::get(Result, Params, false));
  }

#define CONTEXT Context->getContext()
#define HANDLE_API_FUNCTION(Name, Result, ...)                                 \
  Constant *get##Name##Fn() {                                                  \
    return getAPIFunction("LLVM" #Name, Result, {__VA_ARGS__});                \
  }
#include "CAPIFunctions.def"
#undef HANDLE_API_FUNCTION
#undef CONTEXT

  GlobalVariable *createGlobalString(StringRef Str,
                                     const Twine &Name = "mix.name") {
    IRBuilderBase B(Parent->getContext());
    B.SetInsertPoint(Builder->getParent());

    // Does not actually build any instructions.
    return B.CreateGlobalString(Str, Name);
  }

private:
  StagedModule(Function *Parent) : Parent(Parent) {}

  Function *Parent;
  DenseMap<Type *, Instruction *> Types;
};

// This class provides a wrapper around IRBuilder to build code that builds
// other code when executed, using LLVM library API.
template <typename IRBuilder> class StagedIRBuilder {
public:
  StagedIRBuilder(IRBuilder &Builder, StagedModule &SM) : B(Builder), SM(SM) {}
  StagedIRBuilder(const StagedIRBuilder &Other) = delete;
  StagedIRBuilder(StagedIRBuilder &&Other) = default;

  IRBuilder &getBuilder() const { return B; }
  StagedModule &getModule() const { return SM; }

  void setFunction(Instruction *F) { SF = F; }
  Instruction *getFunction() const { return SF; }

  // Interface to particular LLVM API calls.
  Instruction *createFunction(FunctionType *Type,
                              GlobalValue::LinkageTypes Linkage,
                              const Twine &Name = "",
                              const Twine &InstName = "");
  Instruction *setLinkage(Value *Global, GlobalValue::LinkageTypes Linkage);
  Instruction *setName(Instruction *I, StringRef Name);
  Instruction *positionBuilderAtEnd(Instruction *StagedBasicBlock,
                                    const Twine &InstName = "");

  // Stage a type by inserting commands to reconstruct it.
  Instruction *stage(Type *Ty, const Twine &InstName = "");

  // Stage a value by inserting commands to build it, unless it exists, in
  // which case return the previous value. Staging a value that is a basic
  // block means creating an empty basic block in the staged function.
  Instruction *stage(Value *V);
  Instruction *stage(Argument *A, unsigned ArgNo);
  Instruction *stage(std::unique_ptr<Instruction, ValueDeleter> &&I) {
    return stage(I.get());
  }

  // Define static value in the generated function if it is defined elsewhere.
  Value *defineStatic(Value *V);
  Value *defineStatic(Argument *A, Value *SA);
  BasicBlock *defineStatic(BasicBlock *BB);
  Instruction *defineStatic(Instruction *I);
  PHINode *defineStatic(PHINode *Phi, bool Staged);

  // Stage a static value as a constant in generated code.
  Instruction *stageStatic(Value *V);

  // Stage a string by creating a global string variable with a given
  // initializer and returning an `i8*' pointer to the first character.
  Value *stage(StringRef Name, const Twine &VarName = "mix.name") {
    GlobalVariable *GV = SM.createGlobalString(Name, VarName);
    return B.CreateInBoundsGEP(GV->getValueType(), GV,
                               {B.getInt32(0), B.getInt32(0)}, VarName);
  }

  // Change target of a dynamic call.
  void setCalledValue(Instruction *Call, Instruction *V);

private:
  Instruction *stageBasicBlock(BasicBlock *Block);
  void stageIncomingList(PHINode *Phi, Instruction *StagedPhi);
  Instruction *stageInstruction(Instruction *Inst);

  void addStagedValue(Value *V, Instruction *StagedV);

  // Register a callback to be called when the value is staged. If the value
  // has already been staged, call it immediately.
  void whenStaged(Value *V, std::function<void(Instruction *)> Callback);

  IRBuilder &B;
  StagedModule &SM;
  Instruction *SF = nullptr;
  DenseMap<Value *, Instruction *> StagedValues;
  std::unordered_multimap<Value *, std::function<void(Instruction *)>>
      StageCallbacks;
  DenseMap<Argument *, Value *> StaticArguments;
  DenseMap<BasicBlock *, BasicBlock *> StaticBasicBlocks;
  DenseMap<Instruction *, Instruction *> StaticInstructions;
};

template <typename IRBuilder>
StagedModule StagedModule::build(IRBuilder &B, Value *StagedContext,
                                 const Twine &ModuleID) {
  StagedModule SM(B.GetInsertBlock()->getParent());

  SM.Context = StagedContext;
  SM.Builder =
      B.CreateCall(SM.getCreateBuilderInContextFn(), SM.Context, "builder");

  GlobalVariable *MIDGV = SM.createGlobalString(ModuleID.str());
  Value *MID = B.CreateInBoundsGEP(MIDGV->getValueType(), MIDGV,
                                   {B.getInt32(0), B.getInt32(0)}, "moduleid");
  SM.Module = B.CreateCall(SM.getModuleCreateWithNameInContextFn(),
                           {MID, SM.Context}, "module");

  return SM;
}

template <typename IRBuilder> void StagedModule::dispose(IRBuilder &B) {
  B.CreateCall(getDisposeBuilderFn(), Builder);

  Context = nullptr;
  Builder = nullptr;
  Module = nullptr;
  Parent = nullptr;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createFunction(
    FunctionType *Type, GlobalValue::LinkageTypes Linkage, const Twine &Name,
    const Twine &InstName) {
  auto *F = B.CreateCall(SM.getAddFunctionFn(),
                         {SM.Module, stage(Name.str()), stage(Type)}, InstName);

  if (Linkage != GlobalValue::ExternalLinkage) {
    setLinkage(F, Linkage);
  }

  return F;
}

template <typename IRBuilder>
Instruction *
StagedIRBuilder<IRBuilder>::setLinkage(Value *Global,
                                       GlobalValue::LinkageTypes Linkage) {
  LLVMLinkage CAPILinkage;

  switch (Linkage) {
  case GlobalValue::ExternalLinkage:
    CAPILinkage = LLVMExternalLinkage;
    break;

  case GlobalValue::AvailableExternallyLinkage:
    CAPILinkage = LLVMAvailableExternallyLinkage;
    break;

  case GlobalValue::LinkOnceAnyLinkage:
    CAPILinkage = LLVMLinkOnceAnyLinkage;
    break;

  case GlobalValue::LinkOnceODRLinkage:
    CAPILinkage = LLVMLinkOnceODRLinkage;
    break;

  case GlobalValue::WeakAnyLinkage:
    CAPILinkage = LLVMWeakAnyLinkage;
    break;

  case GlobalValue::WeakODRLinkage:
    CAPILinkage = LLVMWeakODRLinkage;
    break;

  case GlobalValue::AppendingLinkage:
    CAPILinkage = LLVMAppendingLinkage;
    break;

  case GlobalValue::InternalLinkage:
    CAPILinkage = LLVMInternalLinkage;
    break;

  case GlobalValue::PrivateLinkage:
    CAPILinkage = LLVMPrivateLinkage;
    break;

  case GlobalValue::ExternalWeakLinkage:
    CAPILinkage = LLVMExternalWeakLinkage;
    break;

  case GlobalValue::CommonLinkage:
    CAPILinkage = LLVMCommonLinkage;
    break;
  }

  return B.CreateCall(
      SM.getSetLinkageFn(),
      {Global, ConstantInt::get(SM.getLinkageTy(), CAPILinkage)});
}

// Set name of a staged value.
template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::setName(Instruction *I,
                                                 StringRef Name) {
  return B.CreateCall(SM.getSetValueNameFn(), {I, stage(Name)});
}

template <typename IRBuilder>
Instruction *
StagedIRBuilder<IRBuilder>::positionBuilderAtEnd(Instruction *StagedBlock,
                                                 const Twine &InstName) {
  return B.CreateCall(SM.getPositionBuilderAtEndFn(), {SM.Builder, StagedBlock},
                      InstName);
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stage(Type *Ty,
                                               const Twine &InstName) {
  Instruction *StagedTy = SM.lookupType(Ty);

  if (StagedTy)
    return StagedTy;

  switch (Ty->getTypeID()) {
  case Type::VoidTyID:
    StagedTy = B.CreateCall(SM.getVoidTypeInContextFn(), SM.Context, InstName);
    break;

  case Type::HalfTyID:
    StagedTy = B.CreateCall(SM.getHalfTypeInContextFn(), SM.Context, InstName);
    break;

  case Type::FloatTyID:
    StagedTy = B.CreateCall(SM.getFloatTypeInContextFn(), SM.Context, InstName);
    break;

  case Type::DoubleTyID:
    StagedTy =
        B.CreateCall(SM.getDoubleTypeInContextFn(), SM.Context, InstName);
    break;

  case Type::X86_FP80TyID:
    StagedTy =
        B.CreateCall(SM.getX86FP80TypeInContextFn(), SM.Context, InstName);
    break;

  case Type::FP128TyID:
    StagedTy = B.CreateCall(SM.getFP128TypeInContextFn(), SM.Context, InstName);
    break;

  case Type::PPC_FP128TyID:
    StagedTy =
        B.CreateCall(SM.getPPCFP128TypeInContextFn(), SM.Context, InstName);
    break;

  case Type::IntegerTyID: {
    unsigned BitWidth = cast<IntegerType>(Ty)->getBitWidth();

    switch (BitWidth) {
    case 1:
    case 8:
    case 16:
    case 32:
    case 64:
    case 128:
      StagedTy = B.CreateCall(
          SM.getAPIFunction(Twine("LLVMInt") + std::to_string(BitWidth) +
                                "TypeInContext",
                            SM.getTypePtrTy(), {SM.getContextPtrTy()}),
          SM.Context, InstName);
      break;

    default:
      StagedTy = B.CreateCall(
          SM.getIntTypeInContextFn(),
          {SM.Context, ConstantInt::get(SM.getUnsignedIntTy(), BitWidth)},
          InstName);
    }

    break;
  }

  case Type::FunctionTyID: {
    auto *FT = cast<FunctionType>(Ty);
    Value *Params;

    if (FT->getNumParams()) {
      Params =
          B.CreateAlloca(SM.getTypePtrTy(), B.getInt32(FT->getNumParams()));

      for (unsigned ParamIndex = 0; ParamIndex < FT->getNumParams();
           ++ParamIndex) {
        B.CreateStore(stage(FT->getParamType(ParamIndex)),
                      B.CreateGEP(Params, B.getInt32(ParamIndex)));
      }
    } else {
      Params =
          ConstantPointerNull::get(PointerType::getUnqual(SM.getTypePtrTy()));
    }

    StagedTy = B.CreateCall(
        SM.getFunctionTypeFn(),
        {stage(FT->getReturnType()), Params,
         ConstantInt::get(SM.getUnsignedIntTy(), FT->getNumParams()),
         ConstantInt::get(SM.getBoolTy(), false)},
        InstName);
    break;
  }

  case Type::PointerTyID: {
    auto *PT = cast<PointerType>(Ty);

    StagedTy = B.CreateCall(
        SM.getPointerTypeFn(),
        {stage(PT->getElementType()),
         ConstantInt::get(SM.getUnsignedIntTy(), PT->getAddressSpace())});
    break;
  }
  }

  assert(StagedTy && "Unsupported type");
  SM.setStagedType(Ty, StagedTy);
  return StagedTy;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stage(Argument *Arg, unsigned ArgNo) {
  assert(!StagedValues.count(Arg) && "Argument has already been staged");

  Instruction *StagedArg = B.CreateCall(
      SM.getGetParamFn(),
      {getFunction(), ConstantInt::get(SM.getUnsignedIntTy(), ArgNo)},
      Arg->getName());

  addStagedValue(Arg, StagedArg);
  return StagedArg;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageBasicBlock(BasicBlock *Block) {
  return B.CreateCall(SM.getAppendBasicBlockInContextFn(),
                      {SM.Context, getFunction(), stage(Block->getName())},
                      Block->getName());
}

// Stage list of incoming values and add them to the already staged Phi node.
// This can't always happen right away because incoming values may not be
// ready yet, so add hooks to build the list when they will be.
template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::stageIncomingList(PHINode *Phi,
                                                   Instruction *StagedPhi) {
  auto *IncomingValueAlloca = B.CreateAlloca(SM.getValuePtrTy(), B.getInt32(1));
  auto *IncomingBlockAlloca =
      B.CreateAlloca(SM.getBasicBlockPtrTy(), B.getInt32(1));

  for (unsigned IncomingNum = 0; IncomingNum < Phi->getNumIncomingValues();
       ++IncomingNum) {
    whenStaged(Phi->getIncomingValue(IncomingNum),
               [=](Instruction *StagedIncomingValue) {
                 B.CreateStore(StagedIncomingValue, IncomingValueAlloca);
                 B.CreateStore(stage(Phi->getIncomingBlock(IncomingNum)),
                               IncomingBlockAlloca);

                 B.CreateCall(SM.getAddIncomingFn(),
                              {StagedPhi, IncomingValueAlloca,
                               IncomingBlockAlloca,
                               ConstantInt::get(SM.getUnsignedIntTy(), 1)});
               });
  }
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageInstruction(Instruction *Inst) {
  SmallVector<Type *, 4> ParamTypes{SM.getBuilderPtrTy()};
  SmallVector<Value *, 4> Arguments{SM.Builder};
  StringRef BuilderName;

  auto pushArg = [&ParamTypes, &Arguments](Type *Ty, Value *V) {
    assert(V->getType() == Ty && "Argument type mismatch");

    ParamTypes.push_back(Ty);
    Arguments.push_back(V);
  };

  auto setBuilderName = [&BuilderName](StringRef Name) {
    assert(BuilderName.empty() && "Builder name is already set");
    BuilderName = Name;
  };

  if (Inst->isBinaryOp()) {
    LLVMOpcode CAPIOpcode = LLVMGetInstructionOpcode(wrap(Inst));

    pushArg(SM.getOpcodeTy(), ConstantInt::get(SM.getOpcodeTy(), CAPIOpcode));
    pushArg(SM.getValuePtrTy(), stage(Inst->getOperand(0)));
    pushArg(SM.getValuePtrTy(), stage(Inst->getOperand(1)));
    pushArg(SM.getCharPtrTy(), stage(Inst->getName()));
    setBuilderName("BinOp");
  } else {
    switch (Inst->getOpcode()) {
    case Instruction::Alloca: {
      auto *Alloca = cast<AllocaInst>(Inst);

      pushArg(SM.getTypePtrTy(), stage(Alloca->getAllocatedType()));
      if (Alloca->isArrayAllocation()) {
        pushArg(SM.getValuePtrTy(), stage(Alloca->getArraySize()));
        setBuilderName("ArrayAlloca");
      } else {
        setBuilderName("Alloca");
      }
      pushArg(SM.getCharPtrTy(), stage(Inst->getName()));
      break;
    }

    case Instruction::Br: {
      auto *Br = cast<BranchInst>(Inst);

      if (Br->isConditional()) {
        pushArg(SM.getValuePtrTy(), stage(Br->getCondition()));
        pushArg(SM.getBasicBlockPtrTy(), stage(Br->getSuccessor(0)));
        pushArg(SM.getBasicBlockPtrTy(), stage(Br->getSuccessor(1)));
        setBuilderName("CondBr");
      } else {
        pushArg(SM.getBasicBlockPtrTy(), stage(Br->getSuccessor(0)));
        setBuilderName("Br");
      }
      break;
    }

    case Instruction::Call: {
      auto *Call = cast<CallInst>(Inst);

      Value *Args = B.CreateAlloca(SM.getValuePtrTy(),
                                   B.getInt32(Call->getNumArgOperands()));

      for (unsigned ArgNum = 0; ArgNum < Call->getNumArgOperands(); ++ArgNum) {
        B.CreateStore(stage(Call->getArgOperand(ArgNum)),
                      B.CreateGEP(Args, B.getInt32(ArgNum)));
      }

      pushArg(SM.getValuePtrTy(), stage(Call->getCalledValue()));
      pushArg(SM.getValuePtrTy()->getPointerTo(), Args);
      pushArg(
          SM.getUnsignedIntTy(),
          ConstantInt::get(SM.getUnsignedIntTy(), Call->getNumArgOperands()));
      pushArg(SM.getCharPtrTy(), stage(Call->getName()));
      setBuilderName("Call");
      break;
    }

    case Instruction::ICmp: {
      LLVMIntPredicate CAPIIntPredicate = LLVMGetICmpPredicate(wrap(Inst));

      pushArg(SM.getIntPredicateTy(),
              ConstantInt::get(SM.getIntPredicateTy(), CAPIIntPredicate));
      pushArg(SM.getValuePtrTy(), stage(Inst->getOperand(0)));
      pushArg(SM.getValuePtrTy(), stage(Inst->getOperand(1)));
      pushArg(SM.getCharPtrTy(), stage(Inst->getName()));
      setBuilderName("ICmp");
      break;
    }

    case Instruction::Load: {
      auto *Load = cast<LoadInst>(Inst);

      pushArg(SM.getValuePtrTy(), stage(Load->getPointerOperand()));
      pushArg(SM.getCharPtrTy(), stage(Inst->getName()));
      setBuilderName("Load");
      break;
    }

    case Instruction::PHI: {
      auto *Phi = cast<PHINode>(Inst);

      pushArg(SM.getTypePtrTy(), stage(Phi->getType()));
      pushArg(SM.getCharPtrTy(), stage(Inst->getName()));
      setBuilderName("Phi");
      break;
    }

    case Instruction::Store: {
      auto *Store = cast<StoreInst>(Inst);

      pushArg(SM.getValuePtrTy(), stage(Store->getValueOperand()));
      pushArg(SM.getValuePtrTy(), stage(Store->getPointerOperand()));
      setBuilderName("Store");
      break;
    }

    case Instruction::Ret: {
      auto *Ret = cast<ReturnInst>(Inst);

      if (auto *ReturnValue = Ret->getReturnValue()) {
        pushArg(SM.getValuePtrTy(), stage(ReturnValue));
        setBuilderName("Ret");
      } else {
        setBuilderName("RetVoid");
      }
      break;
    }

    case Instruction::Trunc: {
      auto *Trunc = cast<TruncInst>(Inst);

      pushArg(SM.getValuePtrTy(), stage(Trunc->getOperand(0)));
      pushArg(SM.getTypePtrTy(), stage(Trunc->getDestTy()));
      pushArg(SM.getCharPtrTy(), stage(Inst->getName()));
      setBuilderName("Trunc");
      break;
    }

    default:
      llvm_unreachable("Unsupported instruction");
    }
  }

  assert(!BuilderName.empty() && "Builder name is not set");

  return B.CreateCall(SM.getAPIFunction(Twine("LLVMBuild") + BuilderName,
                                        SM.getValuePtrTy(), ParamTypes),
                      Arguments, Inst->getName());
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stage(Value *V) {
  Instruction *StagedV = StagedValues.lookup(V);

  if (StagedV)
    return StagedV;

  if (auto *Block = dyn_cast<BasicBlock>(V)) {
    StagedV = stageBasicBlock(Block);
  } else if (auto *Const = dyn_cast<Constant>(V)) {
    StagedV = stageStatic(Const);
  } else if (auto *Inst = dyn_cast<Instruction>(V)) {
    StagedV = stageInstruction(Inst);

    if (auto *Phi = dyn_cast<PHINode>(Inst)) {
      stageIncomingList(Phi, StagedV);
    }
  }

  assert(StagedV && "Unsupported value kind");
  addStagedValue(V, StagedV);
  return StagedV;
}

template <typename IRBuilder>
Value *StagedIRBuilder<IRBuilder>::defineStatic(Argument *A, Value *SA) {
  if (A->getParent() == B.GetInsertBlock()->getParent())
    return A;

  if (SA) {
    assert(!StaticArguments.lookup(A) && "Argument has already been defined");
  } else {
    SA = StaticArguments.lookup(A);
    assert(SA && "Argument has not been defined");
    return SA;
  }

  return StaticArguments[A] = SA;
}

template <typename IRBuilder>
BasicBlock *StagedIRBuilder<IRBuilder>::defineStatic(BasicBlock *BB) {
  if (BB->getParent() == B.GetInsertBlock()->getParent())
    return BB;

  auto *SBB = StaticBasicBlocks.lookup(BB);

  if (SBB)
    return SBB;

  SBB = BasicBlock::Create(B.getContext(), BB->getName(),
                           B.GetInsertBlock()->getParent());
  return StaticBasicBlocks[BB] = SBB;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::defineStatic(Instruction *I) {
  if (I->getFunction() == B.GetInsertBlock()->getParent())
    return I;

  auto *SI = StaticInstructions.lookup(I);

  if (SI)
    return SI;

  assert(!isa<PHINode>(I) && "Static PHI nodes require an explicit type");

  SI = B.Insert(I->clone(), I->getName());

  // Remap operands.
  for (Use &Op : SI->operands()) {
    Op = defineStatic(Op);
  }

  return StaticInstructions[I] = SI;
}

// Static phi has either current or staged type depending on the binding type
// of its operands.
template <typename IRBuilder>
PHINode *StagedIRBuilder<IRBuilder>::defineStatic(PHINode *Phi, bool Staged) {
  if (Phi->getFunction() == B.GetInsertBlock()->getParent())
    return Phi;

  auto *SPhi = cast_or_null<PHINode>(StaticInstructions.lookup(Phi));

  if (SPhi)
    return SPhi;

  Type *Ty = Staged ? SM.getValuePtrTy() : Phi->getType();
  SPhi = B.CreatePHI(Ty, Phi->getNumIncomingValues(), Phi->getName());
  SPhi->moveBefore(B.GetInsertBlock()->getFirstNonPHI());

  for (unsigned IncomingNum = 0; IncomingNum < Phi->getNumIncomingValues();
       ++IncomingNum) {
    Value *IncomingV = Phi->getIncomingValue(IncomingNum);
    BasicBlock *IncomingBB = Phi->getIncomingBlock(IncomingNum);

    if (auto *IncomingI = dyn_cast<Instruction>(IncomingV)) {
      whenStaged(IncomingI, [=](Instruction *StagedI) {
        Value *IncomingV =
            Staged ? StagedI : this->StaticInstructions.lookup(IncomingI);

        SPhi->addIncoming(IncomingV, defineStatic(IncomingBB));
      });
    } else {
      IncomingBB = defineStatic(IncomingBB);

      // Staged value must be live on the edge from the incoming block.
      if (Staged) {
        auto IP = B.saveIP();

        if (auto *Term = IncomingBB->getTerminator()) {
          B.SetInsertPoint(Term);
        } else {
          B.SetInsertPoint(IncomingBB);
        }

        IncomingV = stage(IncomingV);

        B.restoreIP(IP);
      } else {
        IncomingV = defineStatic(IncomingV);
      }

      SPhi->addIncoming(IncomingV, IncomingBB);
    }
  }

  StaticInstructions[Phi] = SPhi;
  return SPhi;
}

template <typename IRBuilder>
Value *StagedIRBuilder<IRBuilder>::defineStatic(Value *V) {
  if (isa<Constant>(V))
    return V;

  if (auto *A = dyn_cast<Argument>(V)) {
    return defineStatic(A, nullptr);
  } else if (auto *BB = dyn_cast<BasicBlock>(V)) {
    return defineStatic(BB);
  } else if (auto *I = dyn_cast<Instruction>(V)) {
    return defineStatic(I);
  }

  llvm_unreachable("Unsupported value kind");
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageStatic(Value *V) {
  Instruction *StagedV = StagedValues.lookup(V);

  if (StagedV)
    return StagedV;

  Value *StaticV = defineStatic(V);

  if (auto *StaticPhi = dyn_cast<PHINode>(StaticV)) {
    addStagedValue(V, StaticPhi);
    return StaticPhi;
  }

  switch (StaticV->getType()->getTypeID()) {
  case Type::VoidTyID: {
    StagedV = nullptr;          // No staged value
    break;
  }

  case Type::HalfTyID:
  case Type::FloatTyID:
  case Type::DoubleTyID:
  case Type::X86_FP80TyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID: {
    Type *Ty = StaticV->getType();
    unsigned BitWidth = Ty->getPrimitiveSizeInBits();

    if (BitWidth <= SM.getDoubleTy()->getPrimitiveSizeInBits()) {
      Value *Double = BitWidth < SM.getDoubleTy()->getPrimitiveSizeInBits()
                          ? B.CreateFPExt(StaticV, SM.getDoubleTy())
                          : StaticV;
      StagedV = B.CreateCall(SM.getConstRealFn(), {stage(Ty), Double},
                             StaticV->getName());
    } else {
      Value *Int = stageStatic(B.CreateBitCast(StaticV, B.getIntNTy(BitWidth)));
      StagedV = B.CreateCall(SM.getConstBitCastFn(), {Int, stage(Ty)},
                             StaticV->getName());
    }
    break;
  }

  case Type::IntegerTyID: {
    IntegerType *Ty = cast<IntegerType>(StaticV->getType());
    unsigned BitWidth = Ty->getBitWidth();

    if (BitWidth <= SM.getUnsignedLongLongIntTy()->getBitWidth()) {
      Value *ULL = BitWidth < SM.getUnsignedLongLongIntTy()->getBitWidth()
                       ? B.CreateZExt(StaticV, SM.getUnsignedLongLongIntTy())
                       : StaticV;
      StagedV = B.CreateCall(
          SM.getConstIntFn(),
          {stage(Ty), ULL, ConstantInt::get(SM.getBoolTy(), false)},
          StaticV->getName());
    } else {
      unsigned NumWords = alignTo(BitWidth, 64) / 64;
      Value *Words = B.CreateAlloca(B.getInt64Ty(), B.getInt32(NumWords));
      Value *Val = StaticV;

      for (unsigned WN = 0; WN < NumWords; ++WN) {
        B.CreateStore(B.CreateTrunc(Val, B.getInt64Ty()),
                      B.CreateGEP(Words, B.getInt32(WN)));
        Val = B.CreateLShr(Val, ConstantInt::get(Ty, 64));
      }

      StagedV = B.CreateCall(
          SM.getConstIntOfArbitraryPrecisionFn(),
          {stage(Ty), ConstantInt::get(SM.getUnsignedIntTy(), NumWords), Words},
          StaticV->getName());
    }
    break;
  }

  case Type::PointerTyID: {
    PointerType *Ty = cast<PointerType>(StaticV->getType());

    Value *IntPtr = B.CreatePtrToInt(StaticV, SM.getUnsignedLongLongIntTy());

    StagedV =
        B.CreateCall(SM.getConstIntToPtrFn(),
                     {B.CreateCall(SM.getConstIntFn(),
                                   {stage(IntPtr->getType()), IntPtr,
                                    ConstantInt::get(SM.getBoolTy(), false)}),
                      stage(Ty)},
                     StaticV->getName());
    break;
  }
  }

  assert((StagedV || StaticV->getType()->isVoidTy()) &&
         "Unsupported static value");
  addStagedValue(V, StagedV);
  return StagedV;
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::addStagedValue(Value *V, Instruction *StagedV) {
  // Call stage callbacks for the value.
  for (auto &SCPair: make_range(StageCallbacks.equal_range(V))) {
    SCPair.second(StagedV);
  }
  StageCallbacks.erase(V);

  StagedValues[V] = StagedV;
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::whenStaged(
    Value *V, std::function<void(Instruction *)> Callback) {
  if (Instruction *StagedV = StagedValues.lookup(V)) {
    Callback(StagedV);
  } else {
    StageCallbacks.insert(std::make_pair(V, Callback));
  }
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::setCalledValue(Instruction *I,
                                                Instruction *V) {
  auto *Call = cast<CallInst>(I);
  assert(Call->getCalledFunction()->getName() == "LLVMBuildCall" &&
         "Not a dynamic call");

  Call->setArgOperand(1, V);
}

} // namespace mix

} // namespace llvm

#endif // LLVM_LIB_TRANSFORMS_MIX_STAGEDIRBUILDER_H
