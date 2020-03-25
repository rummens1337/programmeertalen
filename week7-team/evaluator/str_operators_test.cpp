#include <iostream>
#include <sstream>
using namespace std;

#include "Str.h"

/*! Creates and calls functions/operators on Matrix for test purposes */
int main()
{
    try
    {

        stringstream ss;
        ss<<"1, 2, 3\n";
        ss<<"4, 5, 6\n";
        // ss>>matrix;  // operator>>

        cout<<"matrix:\n"<< 2 <<'\n'; // operator<<
    }
    catch(std::exception& e)
    {
        std::cerr<<"exception caught: "<<e.what()<<'\n';
        exit(1);
    }
    return 0;
}
