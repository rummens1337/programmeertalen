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
        ss<<"1";
        ss<<"+";
        ss<<"2";
        ss << "\n";

        cout<<"STR:\n" <<'\n'; // operator<<
    }
    catch(std::exception& e)
    {
        std::cerr<<"exception caught: "<<e.what()<<'\n';
        exit(1);
    }
    return 0;
}
