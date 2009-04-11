#ifndef MY_LOGGER_H_
#define MY_LOGGER_H_

#include <string>
#include <iostream>
#include <fstream>

const char * const LOG_FILE_NAME = "/tmp/my_axis.log";
enum exceptionState {noException,NotOpenException};

class my_logger
{
private:
	std::ofstream _f;
/*	
#ifdef USINGCOUT
	std::streambuf *m_pOutbuf;
#endif

#ifdef USINGCERR
	std::streambuf *m_pErrbuf;
#endif
*/
public:
	my_logger():_f(LOG_FILE_NAME, std::ios_base::app)
	{
		if(!_f)
			throw NotOpenException;
//			system("pause");
//			exit(1);
			
/*		
#ifdef USINGCOUT
		m_pOutbuf = std::cout.rdbuf(_f.rdbuf());  
		std::cout.setf(std::ios_base::unitbuf);
#endif

#ifdef USINGCERR
		m_pErrbuf = std::cerr.rdbuf(_f.rdbuf()); 
		std::cerr.setf(std::ios_base::unitbuf);
#endif
*/		_f.setf(std::ios_base::unitbuf);
	}
	
	my_logger(std::string filename):_f(filename.c_str(), std::ios_base::app)
	{
/*
#ifdef USINGCOUT
		m_pOutbuf = std::cout.rdbuf(_f.rdbuf());  	
		std::cout.setf(std::ios_base::unitbuf);
#endif

#ifdef USINGCERR
		m_pErrbuf = std::cerr.rdbuf(_f.rdbuf()); 
		std::cerr.setf(std::ios_base::unitbuf);		
#endif
*/		_f.setf(std::ios_base::unitbuf);
	}
	
	template <class T>
	std::ofstream& operator<<(T arg) {
		_f<<arg;
		return _f;
	}
	
	static my_logger& get_logger() {
		try
		{
			static my_logger _l;
			return _l; 						//?
		}
		catch(exceptionState& errObj)
		{
			std::cerr<<"Exception = "<<errObj<<"\n";
		}
	}
	
	~my_logger()
	{
		_f.unsetf(std::ios_base::unitbuf);
/*#ifdef USINGCOUT
		std::cout.rdbuf(m_pOutbuf);
		delete m_pOutbuf;
		m_pOutbuf = NULL;
#endif

#ifdef USINGCERR
		std::cout.rdbuf(m_pErrbuf);
		delete m_pErrbuf;
		m_pErrbuf = NULL;
#endif*/
	}
};

#define mylogger my_logger::get_logger()
#endif //MY_LOGGER_H_
