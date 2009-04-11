#ifndef _TEST_MOVE_FROG_
#define _TEST_MOVE_FROG_

#include "fp.h"
#include "AssertTool.h"

//
// general utilities
//
namespace NLIST{

	//
	// Length
	//
	template <class AList> struct Length{
		static const int value = 1 + Length<AList::Rest>::value;
	};

	template <> struct Length<Empty>{
		static const int value = 0;
	};

	//
	// GetAt
	//
	template <class AList, int i> struct GetAt{
		static const int value = GetAt<AList::Rest, i-1>::value;
	};

	template <class AList> struct GetAt<AList, 0>{
		static const int value=AList::First;
	};

	template <int i> struct GetAt<Empty, i>{
		static const int value = -10000;	//error!
	};

	template <> struct GetAt<Empty, 0>{
		static const int value = -10000;	//error!
	};

	//
	// SetAt
	//
	template <class AList, int i, int value> struct SetAt{
		typedef List<AList::First, 
			typename SetAt<typename AList::Rest, i-1, value>::Result 
		> Result;
	};

	template <class AList, int value> struct SetAt<AList, 0, value>{
		typedef List<value, typename AList::Rest> Result;
	};

	//
	// SwapAt
	//
	template<class AList, int pos1, int pos2> struct SwapAt{
		static const int x = GetAt<AList, pos1>::value;
		static const int y = GetAt<AList, pos2>::value;

		typedef typename SetAt<
			typename SetAt<AList, pos1, y>::Result, pos2, x>::Result Result;
	};

	//
	// Contains? 
	//
	template<class AList, long elem> struct Contains{
		static const bool value = (AList::First == elem) ? 
			true : 
			Contains<typename AList::Rest, elem>::value;
	};

	template<long elem> struct Contains<Empty, elem>{
		static const bool value = false;
	};

	//
	// IsEqualList?
	//
	template <class X, class Y> struct IsEqualList{
		static const bool value = (X::First == Y::First) && IsEqualList<X::Rest, Y::Rest>::value;
	};

	template <class X> struct IsEqualList<X, Empty>{
		static const bool value = false;
	};

	template <class Y> struct IsEqualList<Empty, Y>{
		static const bool value = false;
	};

	template<> struct IsEqualList<Empty, Empty>{
		static const bool value = true;
	};

	//
	// AssertEqualList?
	//
	template<class X, class Y> struct AssertEqualList{
		static const bool value = IsEqualList<X, Y>::value;
		static void print(){
			if(value == true)
				std::cout<<"OK.\n";
			else{
				std::cout<<"Fail.\nX=";
				Print<X>::print();
				std::cout<<"Y=";
				Print<Y>::print();
			}
		}
	};
} //namespace NLIST


namespace IntUtil{
	//
	//const
	//
	const long startLine =1112333;	//x[n-1], x[n-2], ..., x[1], x[0]
	const long targetLine=3332111;
	const int LENGTH = 7;

	//
	// GetAt
	//
	template<long line, int i> struct GetAt{
		static const int value = GetAt<line/10, i-1>::value;
	};

	template<long line> struct GetAt<line, 0>{
		static const int value = line % 10;
	};

	//
	// SetAt
	//
	template<long line, int i, int v> struct SetAt{
		static const int value = SetAt<line/10, i-1, v>::value*10 + (line % 10);
	};

	template<long line, int v> struct SetAt<line, 0, v>{
		static const int value = (line / 10)*10+v;
	};

	//
	// Find
	//
	template<long line, int v> struct Find{
		static const int value = ( line % 10 ) == v ? 0 : (1 + Find<line/10, v>::value);
	};

	template<int v> struct Find<0, v>{
		static const int value = ( v == 0) ? 0 : -1000;	//error
	};

	//
	// SwapAt
	//
	template<long line, int pos1, int pos2> struct SwapAt{
		static const long value = SetAt<
			SetAt<line, pos1, GetAt<line, pos2>::value>::value,
			pos2, GetAt<line, pos1>::value>::value;
	};

	//
	// IsEqualAt?
	//
	template<long line, int pos, int v> struct IsEqualAt{
		static const bool value = (GetAt<line, pos>::value == v);
	};

	//
	// Comparison operators
	//
	template<int x, int y> struct Less{
		static const bool value = (x<y);
	};

	template<int x, int y> struct Greater{
		static const bool value = (x>y);
	};

	template<int x, int y> struct Equal{
		static const bool value = (x==y);
	};

} //namespace IntUtil


namespace MoveFrogs{
	//
	// CanMove?
	//
	template<
		long line, 
		template<int, int> class Op, int pos1, int limit,
		int pos2, int shouldBe
	>
	struct CanMove{
	private:
		//
		// IsPosOK?
		//
		template< template<int, int> class Op, int pos, int limit> struct IsPosOK{
			static const bool value = Op<pos, limit>::value;
		};

		//
		// IsForwardAnd?
		//
		template<bool IsPosOK, long line, int pos, int v> struct IsForwardAnd;

		template<long line, int pos, int v> struct IsForwardAnd<true, line, pos, v>{
			static const bool value = (IntUtil::IsEqualAt<line, pos, v>::value);
		};

		template<long line, int pos, int v> struct IsForwardAnd<false, line, pos, v>{
			static const bool value = false;
		};

	public:
		static const bool value = IsForwardAnd<	
			IsPosOK<Op, pos1, limit>::value,
			line, pos2, shouldBe
		>::value;
	};

	template<
		bool canMove, 
		long line, int pos1, int pos2,
		class Steps
	> 
	struct Move{
		typedef List<IntUtil::SwapAt<line, pos1, pos2>::value, Steps> Result;
	};

	template<long line, int pos1, int pos2, class Steps> 
	struct Move<false, line, pos1, pos2, Steps>{
		typedef Steps Result;
	};

	//
	// StepLeft
	// Steps: a NList contains Result
	//
	template<long line, class Steps> struct StepLeft{
	private:
		static const int pos = IntUtil::Find<line, 2>::value;

	public:
		typedef typename Move<
			CanMove<line, IntUtil::Greater, pos, 0, pos-1, 3>::value,
			line, pos, pos-1, Steps
		>::Result Result;
		
	};

	//
	// JumpLeft
	//
	template<long line, class Steps> struct JumpLeft{
	private:
		static const int pos = IntUtil::Find<line, 2>::value;

	public:
		typedef typename Move<
			CanMove<line, IntUtil::Greater, pos, 1, pos-2, 3>::value,
			line, pos, pos-2, Steps
		>::Result Result;
	};

	//
	// StepRight
	//
	template<long line, class Steps> struct StepRight{
	private:
		static const int pos = IntUtil::Find<line, 2>::value;

	public:
		typedef typename Move<
			CanMove<line, IntUtil::Less, pos, IntUtil::LENGTH-1, pos+1, 1>::value,
			line, pos, pos+1, Steps
		>::Result Result;
	};

	//
	// JumpRight
	//
	template<long line, class Steps> struct JumpRight{
	private:
		static const int pos = IntUtil::Find<line, 2>::value;

	public:
		typedef typename Move<
			CanMove<line, IntUtil::Less, pos, IntUtil::LENGTH-2, pos+2, 1>::value,
			line, pos, pos+2, Steps
		>::Result Result;
	};

	//
	// ChangeIt
	//
	template<long line> struct ChangeIt{
	private:
		struct Try{
			typedef 
				typename JumpRight<
					line,
					typename StepRight< 
						line,
						typename JumpLeft<
							line,
							typename StepLeft<line, Empty>::Result
						>::Result
					>::Result
				>::Result Result;
		};

	public:
		//final result
		typedef typename Try::Result Result;
	};

	//
	// Unique
	// remove elements which can be found in both AList and Src
	//
	template<class Src, class AList> struct Unique{
	private:
		template<bool DoesContain1stElem> struct If;
		template<> struct If<true> {
			typedef typename Unique<typename Src::Rest, AList>::Result Result;
		};

		template<> struct If<false>{
			typedef List<
				Src::First,
				typename Unique<typename Src::Rest, AList>::Result
			> Result;
		};
	public:
		typedef typename If<(NLIST::Contains<AList, Src::First>::value)>::Result Result;
	};

	template<class Records> struct Unique<Empty, Records>{
		typedef Empty Result;
	};

	//
	// main programs
	// ================================================================
	//

	//
	// forward declarations
	//
	template<class Steps, class Tried> struct MoveIt;
	template<class Candidates, class Steps, class Tried> struct TryIt;

	//
	// BackTrack
	// If (FoundSolution) Then (backtrack) Else (return Solution)
	//
	template<
		bool FoundSolution, class Solution,
		class Steps, class Tried
	> struct BackTrack;

	// Yes, has found solution. need not back-track
	template<class Solution, class Steps, class Tried> 
	struct BackTrack<true, Solution, Steps, Tried>{
		static const bool value = true;
		typedef Solution Result;
	};

	// No, can not find solution. need back-track
	template<class Solution, class Steps, class Tried> 
	struct BackTrack<false, Solution, Steps, Tried>{
		typedef MoveIt<
			typename Steps::Rest, 
			List<Steps::First, Tried> 
		> BackTrackResult;
		static const bool value = BackTrackResult::value;
		typedef typename BackTrackResult::Result Result;
	};

	//
	// TryMove
	//
	template<bool OnTarget, class Steps, class Tried> struct TryMove;

	// Yes, It is target. solution is found.
	template<class Steps, class Tried> struct TryMove<true, Steps, Tried>{
		typedef Steps Result;
		static const bool value = true;
	};

	// No, It is not target, need try further steps
	template<class Steps, class Tried> struct TryMove<false, Steps, Tried>{
		typedef TryIt< 
			typename Unique<
				typename ChangeIt<Steps::First>::Result, 
				Tried
			>::Result,
			Steps,
			Tried
		> TryFurtherStepsResult;

		typedef BackTrack<
			TryFurtherStepsResult::value,
			typename TryFurtherStepsResult::Result,
			Steps,
			Tried
		> BackTrackResult;
		typedef typename BackTrackResult::Result Result;
		static const bool value = BackTrackResult::value;
	};

	//
	// MoveIt
	//
	template<class Steps, class Tried> struct MoveIt{
		typedef TryMove< 
			(Steps::First == IntUtil::targetLine),
			Steps, Tried
		> MoveResult;
		typedef typename MoveResult::Result Result;
		static const bool value = MoveResult::value;
	};

	template<class Tried> struct MoveIt<Empty, Tried>{
		static const bool value = false;	//no solution
		typedef Empty Result;
	};

	//
	// TryOthers
	//
	template<
		bool Try1stOK, 
		class Try1stResult,
		class Candidates, class Steps, class Tried
	> struct TryOthers;

	// Try 1st OK, needn't try others
	template<
		class Try1stResult, 
		class Candidates, class Steps, class Tried
	> 
	struct TryOthers<true, Try1stResult, Candidates, Steps, Tried>{
		static const bool value = true;
		typedef Try1stResult Result;
	};

	// Try 1st Failed, must try others
	template<
		class Try1stResult,
		class Candidates, class Steps, class Tried
	> 
	struct TryOthers<false, Try1stResult, Candidates, Steps, Tried>{
		typedef TryIt<typename Candidates::Rest, Steps, Tried> TryOthersResult;
		static const bool value = TryOthersResult::value;
		typedef typename TryOthersResult::Result Result;
	};
	
	//
	// TryIt?
	// Try to find solution based on current steps.
	// If OK, return <true, solution>
	// Else   return <false, empty>
	//
	template<class Candidates, class Steps, class Tried> struct TryIt{
		typedef MoveIt<
			List<Candidates::First, Steps>,
			List<Candidates::First, Tried>
		> Try1stResult;

		typedef TryOthers<
			Try1stResult::value,
			typename Try1stResult::Result,
			Candidates, Steps, Tried
		> TryOthersResult;

		static const bool value = TryOthersResult::value;
		typedef typename TryOthersResult::Result Result;
	};

	template<class Steps, class Tried> struct TryIt<Empty, Steps, Tried>{
		static const bool value = false;
		typedef Empty Result;
	};

	//
	//main core
	//
	template <long startLine> struct MoveFrog{
		typedef MoveIt<
			List<startLine, Empty>, 
			List<startLine, Empty> 
		> MoveResult;

		static const bool value = MoveResult::value;
		typedef typename MoveResult::Result Result;
	};

	//
	// Print Result Steps
	//
	template<class Steps> struct PrintSteps{
	private:
		template<class ReverseSteps> struct ReversePrint{
			static void printDigit(long rawLine){
				if(rawLine < 10){
					std::cout<<rawLine-2<<", ";
				}
				else{
					printDigit(rawLine/10);
					std::cout<<(rawLine % 10)-2<<", ";
				}
			}

			static void print(){
				long rawLine = ReverseSteps::First;
				printDigit(rawLine);
				std::cout<<"\n";
				ReversePrint<typename ReverseSteps::Rest>::print();
			}
		};

		template<> struct ReversePrint<Empty>{
			static void print(){std::cout<<"\n";}
		};
	public:
		static void print(){
			ReversePrint<typename Reverse<Steps>::Result>::print();
		}
	};

} //namespace MoveFrogs

class TestMoveFrog : public TestCase
{
	void test(){
		TestScript<TestMoveFrog>::begin(this)
			<<&TestMoveFrog::testEqual
			<<&TestMoveFrog::testGetSet
			<<&TestMoveFrog::testLength
			<<&TestMoveFrog::testFindPos
			<<&TestMoveFrog::testSwapLine
			<<&TestMoveFrog::testStepLeft
			<<&TestMoveFrog::testJumpLeft
			<<&TestMoveFrog::testChangeIt
			<<&TestMoveFrog::testUnique
			<<&TestMoveFrog::testRun
			<<end;
	}

	void testEqual(){
		std::cout<<__FUNCTION__"\n";
		assertTrue(1112333 == IntUtil::startLine);
		assertTrue(1112333 != IntUtil::targetLine);
	}

	void testGetSet(){
		std::cout<<__FUNCTION__"\n";
		assertTrue(IntUtil::GetAt<1112333, 0>::value == 3);
		assertTrue(IntUtil::GetAt<1112333, 3>::value == 2);
		assertTrue(IntUtil::GetAt<1112333, 6>::value == 1);
		assertTrue(IntUtil::GetAt<1112333, 7>::value == 0);

		assertEqual(IntUtil::SetAt<1112333, 0, 4>::value, 1112334);
		assertEqual(IntUtil::SetAt<1112333, 3, 4>::value, 1114333);
		assertEqual(IntUtil::SetAt<1112333, 6, 4>::value, 4112333);
	}

	void testLength(){
		std::cout<<__FUNCTION__"\n";
		assertEqual(IntUtil::LENGTH, 7);
	}

	void testFindPos(){
		std::cout<<__FUNCTION__"\n";
		assertEqual(IntUtil::Find<1112333, 2>::value , 3);
		assertEqual(IntUtil::Find<1112333, 3>::value , 0);
		assertTrue(IntUtil::Find<1112333, 4>::value < 0 );
	}

	void testSwapLine(){
		std::cout<<__FUNCTION__"\n";
		assertEqual<long>(IntUtil::SwapAt<1112333, 2, 3>::value, 1113233);
		assertEqual<long>(IntUtil::SwapAt<1112333, 1, 3>::value, 1113323);
	}

	void testStepLeft(){
		std::cout<<__FUNCTION__"\n";
		NLIST::AssertEqualList<
			typename MoveFrogs::StepLeft<IntUtil::startLine, Empty>::Result,
			List<1113233, Empty>
		>::print();

		NLIST::AssertEqualList<
			typename MoveFrogs::StepLeft<1113323, Empty>::Result,
			List<1113332, Empty>
		>::print();

		NLIST::AssertEqualList<
			typename MoveFrogs::StepLeft<1113332, Empty>::Result,
			Empty
		>::print();

		NLIST::AssertEqualList<
			typename MoveFrogs::StepLeft<1133321, Empty>::Result,
			Empty
		>::print();
		
	}

	void testJumpLeft(){
		std::cout<<__FUNCTION__"\n";
		NLIST::AssertEqualList<
			typename MoveFrogs::JumpLeft<1112333, Empty>::Result,
			List<1113323, Empty>
		>::print();

		NLIST::AssertEqualList<
			typename MoveFrogs::JumpLeft<1133213, Empty>::Result,
			List<1133312, Empty>
		>::print();

		NLIST::AssertEqualList<
			typename MoveFrogs::JumpLeft<1113332, Empty>::Result,
			Empty
		>::print();

		NLIST::AssertEqualList<
			typename MoveFrogs::JumpLeft<1133231, Empty>::Result,
			Empty
		>::print();

		NLIST::AssertEqualList<
			typename MoveFrogs::JumpLeft<1112333, List<1112333, Empty> >::Result,
			List<1113323, List<1112333, Empty> >
		>::print();
	}

	void testChangeIt(){
		std::cout<<__FUNCTION__"\n";
		NLIST::AssertEqualList<
			typename MoveFrogs::ChangeIt<1112333>::Result,
			List<1211333, List<1121333, List<1113323, List<1113233, Empty> > > >
		>::print();

		NLIST::AssertEqualList<
			typename MoveFrogs::ChangeIt<1113323>::Result,
			List<1113332, Empty>
		>::print();
	}

	void testUnique(){
		std::cout<<__FUNCTION__"\n";
		NLIST::AssertEqualList<
			typename MoveFrogs::Unique< 
				List<1112333, List<1113233, Empty> >,
				Empty
			>::Result,
			List<1112333, List<1113233, Empty> >
		>::print();

		NLIST::AssertEqualList<
			typename MoveFrogs::Unique< 
				List<1112333, List<1113233, Empty> >,
				List<1112333, List<1113444, Empty> >
			>::Result,
			List<1113233, Empty>
		>::print();

		NLIST::AssertEqualList<
			typename MoveFrogs::Unique< 
				List<1112333, List<1113233, Empty> >,
				List<1112333, List<1113233, Empty> >
			>::Result,
			Empty
		>::print();
	}

	void testRun(){
		typedef MoveFrogs::MoveFrog<IntUtil::startLine> MoveResult;
		if(MoveResult::value == true){
			std::cout<<"Find a result: \n";
			MoveFrogs::PrintSteps<typename MoveResult::Result>::print();
		}
		else
			std::cout<<"No solution\n";
		std::cout<<"===========================\n";
	}
};

#endif