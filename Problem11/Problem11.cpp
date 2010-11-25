// Problem11.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "grid.h"

#include <list>
#include <iostream>
using namespace std;



int SumFourCellsTwentySize(int iOriginX,int iOriginY,int iWidthHeight,int iDistance,const int pArr[20][20]) 
{
	list<int> sums;
	
	if(iOriginX-iDistance > 0)
	{
		int iSumLeft = pArr[iOriginX][iOriginY] * pArr[iOriginX-1][iOriginY] * pArr[iOriginX-2][iOriginY]  * pArr[iOriginX-3][iOriginY];
		sums.push_back(iSumLeft);
	}

	if(iOriginX+iDistance < iWidthHeight)
	{
		int iSumRight = pArr[iOriginX][iOriginY] * pArr[iOriginX+1][iOriginY] * pArr[iOriginX+2][iOriginY]  * pArr[iOriginX+3][iOriginY];
		sums.push_back(iSumRight);
	}

	if(iOriginY+iDistance < iWidthHeight)
	{
		int iSumUp = pArr[iOriginX][iOriginY] * pArr[iOriginX][iOriginY+1] * pArr[iOriginX][iOriginY+2]  * pArr[iOriginX][iOriginY+3];
		sums.push_back(iSumUp);
	}

	if(iOriginY-iDistance > 0)
	{
		int iSumDown = pArr[iOriginX][iOriginY] * pArr[iOriginX][iOriginY-1] * pArr[iOriginX][iOriginY-2]  * pArr[iOriginX][iOriginY-3];
		sums.push_back(iSumDown);
	}

	if(iOriginY-iDistance > 0 && iOriginX+iDistance < iWidthHeight)
	{
		int iDiagDownRight = pArr[iOriginX][iOriginY] * pArr[iOriginX+1][iOriginY-1] * pArr[iOriginX+2][iOriginY-2]  * pArr[iOriginX+3][iOriginY-3];
		sums.push_back(iDiagDownRight);
	}

	if(iOriginY+iDistance < iWidthHeight && iOriginX+iDistance < iWidthHeight)
	{
		int iDiagUpRight = pArr[iOriginX][iOriginY] * pArr[iOriginX+1][iOriginY+1] * pArr[iOriginX+2][iOriginY+2]  * pArr[iOriginX+3][iOriginY+3];
		sums.push_back(iDiagUpRight);
	}

	if(iOriginY+iDistance > 0 && iOriginX-iDistance > 0)
	{
		int iDiagDownLeft = pArr[iOriginX][iOriginY] * pArr[iOriginX-1][iOriginY-1] * pArr[iOriginX-2][iOriginY-2]  * pArr[iOriginX-3][iOriginY-3];
		sums.push_back(iDiagDownLeft);
	}

	if(iOriginY+iDistance < iWidthHeight && iOriginX-iDistance > 0)
	{
		int iDiagUpLeft = pArr[iOriginX][iOriginY] * pArr[iOriginX-1][iOriginY+1] * pArr[iOriginX-2][iOriginY+2]  * pArr[iOriginX-3][iOriginY+3];
		sums.push_back(iDiagUpLeft);
	}

	sums.sort();
	sums.reverse();
	

	return sums.front();
}


int _tmain(int argc, _TCHAR* argv[])
{
	int x = iSampleGrid_large[0][0];

	for(int i = 0; i < 20; i++)
	{
		for(int j=0;j<20;j++)
		{
			int sum = SumFourCellsTwentySize(i,j,20,4,iSampleGrid_large);
			
			if(sum > x)
			{
				x = sum;
			}

		}
	}
	
	cout << "sum is "<<x << endl;

	return 0;
}

