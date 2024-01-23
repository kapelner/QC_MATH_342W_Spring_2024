#include <Rcpp.h>
using namespace Rcpp;


//NOTE: this is a private function and it is not exported
int partition(NumericVector a, int l, int u)
{
    double v, temp;
    int i, j;

    v = a[l];
    i = l;
    j = u + 1;
    
    do
    {
        do
            i++;
            
        while (a[i] < v && i <= u);
        
        do
            j--;
        while (v < a[j]);
        
        if (i < j)
        {
            temp = a[i];
            a[i] = a[j];
            a[j] = temp;
        }
    } while (i < j);
    
    a[l] = a[j];
    a[j] = v;
    
    return j;
} 


//NOTE: this is a private function and it is not exported
void quicksort_internal(NumericVector a, int l, int u)
{
    int j;
    if (l < u)
    {
        j = partition(a, l, u);
        quicksort_internal(a, l, j - 1);
        quicksort_internal(a, j + 1, u);
    }
}

// [[Rcpp::export]]
NumericVector quicksort_cpp(NumericVector a) 
{ 
    quicksort_internal(a, 0, a.size() - 1); 
    return a;
} 