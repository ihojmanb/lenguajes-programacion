#include <iostream>
using namespace std;

long naive_f (long n)
{
    if (n == 0)
    {
        return 0;
    }
    else
    {
        short c = (n % 2 == 0) ? 1 : -1;
        return naive_f(n - 1) + c * n;
    }
}

// PARTE a)
// ¿Qué valor observó que “agota” la pila?
// el valor que genera un stack overflow es 174.530


// PARTE b)
// función auxiliar llamada por smart_f, que convierte naive_f en
// en una funcion recursiva por cola
long aux_smart (long n, long accum){
  if (n == 0){
      return accum;
  }
  else{
      short c = (n % 2 == 0) ? 1 : -1;
      return aux_smart(n - 1, accum + c*n);
  }
}

long smart_f (long n){
  return aux_smart(n, 0);
}




int main ()
{
    long number, res;
    cout << "Please enter a natural number: ";
    cin >> number;
    // res = naive_f(number);
    res = smart_f(number);
    cout << "Its image through f is: " << res << ".\n";
    return 0;
}
