// ModN.java (part of FunDomain java program)
// this class is for working with integers mod N


/* --------------------------------------------------------------

   FunDomain: Program for drawing fundamental domains of subgroups of SL_2(Z)
   Copyright (C) 2001  Helena A. Verrill

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

   Helena A. Verrill

   UK address:
   23 Roper Close
   Rugby
   CV21 4PF
   England
   email: verrill@math.ku.dk

   see web page: http://hverrill.net
   for most recent address and email address.   

   more information about the GPL can be found at:
   http://www.gnu.org/copyleft/gpl.html

------------------------------------------------------------------
*/

public class ModN{

    public int N;
    public int phi;
    public int[] invert = new int[500];
    // above line means max N will be 500
    public int ord; //order of (Z/NZ)*

    public ModN(int N){
	this.N = N;
	makeInv();
	
	int i;
	ord = 0;
	for (i=1;i<N;i++){
	    if (invert[i]!=-1) ord++; 
	}
    
    }
    
    // unfortunately this will read as a
    // somewhat backwards notation.
    // N.mod(a) will mean a mod N.

    public int mod(int a){
	int A;
	A = a-(a/N)*N;
	if (A<0) A = N+A;
	return A;
    }
    
    public int mult(int a, int b){
	int A = mod(a);
	int B = mod(b);
	return mod(A*B);
    }

    
    private void makeInv(){
	int i,j,u=0;
	boolean found = false;
	// set to -1 if not invertible
	invert[0]=-1;
	invert[1]=1;
	for (i=2;i<=N/2;i++){
	    if (invert[i]!=-1){
		// if i is a factor, get rid of multiples
		if ( (N/i)*i==N ) 
		    {for(j=1;j<N/i;j++) {invert[i*j]=-1;}}
		else {
		    found = false;
		    j=0;
		    while(j<i-1 && !found){
			j++;
			u = (N*j+1);
			if ((u/i)*i==u) found=true;
		    }
		    if (found) {
			invert[i] = u/i;
			invert[u/i] = i;
			invert[N-i] = N-u/i;
			invert[N-u/i] = N-i;
		    }
		}
	    }
	}
    }
    
    
    public boolean gen(int a) {
	// this will determine if a generates (Z/NZ)
	// this is a pretty stupid algorithm at the moment
	
	int A = mod(a);
	int i;
	int B = A;

	if (A==1 || A==-1) return false;
	if (invert[A]==-1) return false;
	else { 
	    for (i=1;i<=ord/2;i++){
		B = mod(A*B);
		if (B==1) return false;
	    }
	}
	return true;
    }

}
 
    
















