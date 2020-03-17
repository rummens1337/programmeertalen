#include "evaluator.h"
#include "Matrix.h"
#include "MatrixT.h"
#include "Str.h"


int main()
{
    try
    {
        Evaluator<double,0> evaluator;
        std::cout<<evaluator.evaluate(std::cin)<<'\n';
    }
    catch(std::exception& e)
    {
        std::cerr<<"exception caught: "<<e.what()<<'\n';
        exit(1);
    }
    return 0;
}
