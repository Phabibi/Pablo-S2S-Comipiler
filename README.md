# Pablo S2S Compiler
 -An S2S Compiler for the Prabaixs Pablo language 

 -Implemented an S2S Compiler for the Parabix technology using haskell and C++ . Parabix is a high-performance programming framework for streaming text processing applications, leveraging both SIMD and multicore parallel processing features.

 -Pablo is a Python subset language that allows you to prototype parallel bit stream programs in Python based on the concept of unbounded (arbitrary-length) bit streams.

 -Since Pablo uses C++ to be compiled/executed , as a research project for my functional programming course , I was given the task to implement a solution in order to skip the need of having to translating the Pablo code into C++ by hand.

  -The transcompiler takes a Pablo code, then translates it into Haskell representation using recursive descent parsing and finally produces the C++ equivalent code that is used by the Parabix compiler

# Pablo Language
  - http://parabix.costar.sfu.ca/
# Requirments
  - download haskell ```https://www.haskell.org/downloads/```
  
# How to use 
  - run the repl ``` $ ghci ```
  - ``` showHead <Pablo Code> ```
# Example 
```showHead "kernel u8u16 :: <i1>[8] u8basi -> <i1>[18] selection
 {nonFinal = ~u8final
   Initial = InFile(~Advance(nonFinal, 1))
   ASCII = u8final & Initial
   lookAheadFinal = Lookahead(u8final, 1)
   secondLast = lookAheadFinal & nonFinal
   u8mask6  = InFile(secondLast | ASCII)
   prefix2 = secondLast & Initial
   lookAhead2 = Lookahead(u8final, 2)
   thirdLast = lookAhead2 & nonFinal & ~secondLast
   u8mask12 = InFile(thirdLast | prefix2 | ASCII)
}"
```
### will produce :
```
class u8u16Kernel final: public pablo::PabloKernel {
public:
    u8u16Kernel(const std::unique_ptr<kernel::KernelBuilder> & b,
StreamSet*  u8basi,
StreamSet*  selection);
 bool isCachable() const override { return true; }
 bool hasSignature() const override {return false;}
 void generatePabloMethod() override;
};
u8u16Kernel::u8u16Kernel(const std::unique_ptr<kernel::KernelBuilder> & b ,{Binding{"u8basi",u8basi}},
{Binding{"selection",selection}})

Voidu8u16Kernel::generatePabloMethod(){
PabloBuilder main(getEntryScope());
std::vector <PabloAST*>u8basi= getInputStreamSet("u8basi");
PabloAST * selection[18];
{
auto nonFinal = main.createVar("nonFinal");
main.createAssign( nonFinal,main.createNot(u8final) );
}
{
auto Initial = main.createVar("Initial");
main.createAssign( Initial,main.createInFile(main.createNot(main.createAdvance(nonFinal,main.getInteger(1))) ));
}
{
auto ASCII = main.createVar("ASCII");
main.createAssign( ASCII,main.createAnd(u8final,Initial));
}
{
auto lookAheadFinal = main.createVar("lookAheadFinal");
main.createAssign( lookAheadFinal,main.createLookahead(u8final,main.getInteger(1)));
}
{
auto secondLast = main.createVar("secondLast");
main.createAssign( secondLast,main.createAnd(lookAheadFinal,nonFinal));
}
{
auto u8mask6 = main.createVar("u8mask6");
main.createAssign( u8mask6,main.createInFile(main.createOr(secondLast,ASCII)));
}
{
auto prefix2 = main.createVar("prefix2");
main.createAssign( prefix2,main.createAnd(secondLast,Initial));
}
{
auto lookAhead2 = main.createVar("lookAhead2");
main.createAssign( lookAhead2,main.createLookahead(u8final,main.getInteger(2)));
}
{
auto thirdLast = main.createVar("thirdLast");
main.createAssign( thirdLast,main.createAnd(main.createAnd(lookAhead2,nonFinal),main.createNot(secondLast) ));
}
{
auto u8mask12 = main.createVar("u8mask12");
main.createAssign( u8mask12,main.createInFile(main.createOr(main.createOr(thirdLast,prefix2),ASCII)));
}
for (unsigned i = 0 ; i < 18;i++) {
 pb.createAssign(pb.createExtract(getOutputStreamVar("selection")pb.getInteger(i)), [18]);
}
}
```
