#include <iostream>
using namespace std;

long double_rec(long accum, long value){
  if(value == 0){
    return accum;
  }
  else{
    return double_rec(accum + 2, value - 1);
  }
}

int main()
{
    long number, res;
    cout << "Please enter a natural number: ";
    cin >> number;
    res = double_rec(0, number);
    cout << "Its double is " << res << ".\n";
    return 0;
}
