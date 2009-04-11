#ifndef _JAL_COMMON_H_
#define _JAL_COMMON_H_

#define DBG_MARK	__FILE__<<":"<<__LINE__<<"==>"

namespace jal {

//
// Resouce with ID holder Policy
// 1. provide ID type
// 2. provide Data type
// 3. provide get_id() method
//

//
// Resouce Operation Policy
// 1. provide eq()
// 2. provide lt()
// 3. provide diff()
//

template<class T> struct IndexOp{
	bool eq(const T& v1, const T& v2) const { return v1.get_id() == v2.get_id(); }
	bool lt(const T& v1, const T& v2) const { return v1.get_id() < v2.get_id(); }
	typename T::ID diff(const T& v1, const T& v2) const { return v1.get_id() - v2.get_id(); }
};

//
// ResHolder:	Resouce with ID Holder
// Op:			policy:	Strict weak order policy
//

template< class DataHolder, template<class> class Op = IndexOp >
class resource : public DataHolder, public Op<DataHolder>
{
private:
	typedef resource<DataHolder, Op> 	_self; 
	typedef typename DataHolder::Data	_Data;
	
public:
	resource(){}
	resource(const _Data& v): DataHolder(v) {}
	_self& operator=(const _Data& v){
		DataHolder::operator=(v);
		return *this;
	}
			
	const bool operator == (const _self& v) const { return eq(*this, v); }
	const bool operator <  (const _self& v) const { return lt(*this, v); } 		
	typename DataHolder::ID operator - (const _self& v) const { return diff(*this, v); }

	// To get other data, just use up-cast to <Data>
};

}//end namespace
		
#endif //_JAL_COMMON_H_
