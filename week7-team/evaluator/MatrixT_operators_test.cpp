#include <iostream>
#include <sstream>
using namespace std;

#include "MatrixT.h"
#include "Str.h"

/*! Creates and calls functions/operators on MatrixT<T> for test purposes */
int main()
{
    try
    {

        MatrixT<Str> matrix;

        stringstream ss;
        ss<<"a, b\n";
        ss<<"c, d\n";
        ss>>matrix;  // operator>>

        cout<<"matrix:\n"<< matrix <<'\n'; // operator<<

        cout<<"-matrix:\n"<< -matrix <<'\n';

        cout<<"transpose(matrix):\n"<< transpose(matrix) <<'\n';

        cout<<"matrix+matrix:\n"<< matrix+matrix <<'\n';

        cout<<"matrix-matrix:\n"<< matrix-matrix <<'\n';

        cout<<"matrix*transpose(matrix):\n"<< matrix*transpose(matrix) <<'\n';

    }
    catch(std::exception& e)
    {
        std::cerr<<"exception caught: "<<e.what()<<'\n';
        exit(1);
    }
    return 0;
}
