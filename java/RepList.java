// RepList.java (part of FunDomain java program)

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



import java.awt.*;
import java.awt.event.*;
import java.applet.*;

/*
 *  This is to define a list of Coset represenatives
 *
 */

// Currently, the way this works, is that there can be at most 2000 
// different coset reps, ie, index at most 2000.
// (this should be corrected to allow arbitrary length in a future version.

// The list is given by "reps"
// There is a table, "table", which for each possible reps entrie says
// 0: whether it has been filled up
// 1: what is the T adjacent thing in reps 
// 2: what is the Tm adjacent thing
// 3: what is the S adjacent things
// There is also a 'current length', which says how far the list of
// reps has been filled up.
// Have to make this a two dimensional thing; the zeroth entrie in
// each case tells whether or not we'e tried to see if anything is
// adjacent, etc.
// have to make a _3_ dimensional thing, with next entry saying
// whether there is really something next to it, or if the thing
// is further away.

// example:
// table[i][][]  means that for the ith conjclass in the rep list,
// table[i][1][0] = 1; we've looked in T direction
// table[i][1][1] = j; the adjacetent thing is in reps[j]
// table[i][1][2] = 1; it really is next to it, not just equiv to next to.

//***(
// could replace this with an object with 
// attributes:
// Tdirection.lookedin  (boolean)
// Tdirection.linksto   (int) 
// Tdirection.gluedto   (boolean)
// and there would be also Rdirection and Sdirection
// table[i] would be an object with these 3 dirctions
// maybe call it 'triangle'.
// ***)

// The initial rep list will just be the identity
// there will be a command to tell whether or not the thing is
// a complete list of coset reps, and
// to tell the length

// we will want to add a command to make a better looking set of reps
// for drawing

// cusps array contains list of cusps, 
// eg, for n'th cusp at 1/2 of width 3, find (1,2,3) as nth entry
// ie, in this case cusps[n][0] = 1, cusps[n][1] = 2, cusps[n][2] = 3
// add cusps[n][4] = i to mean a rep for this cusp, in reps[i].
// maximum of 500 cusps.


// there should be several different possible ways to draw the domain
// there is the basic method, which is implemented first
// next, the 'bouquet' method (which I'll call 'gathered' because
// that's easier to spell.
// for this we just force the things to be added by applying 'T'.
// there is not yet a button to turn this option on and off, so at the
// moment it's set to be off.

public class RepList{

    public ConjClassRep[] reps = new ConjClassRep[2000];
    public int[][][] table =new int[2000][4][3] ;
    public String grouptype = new String();
    public int[] dist = new int[2000];
    public int[][] cusps = new int[500][4];
    public int CuspNo;
    public int genus;
    public int N;
    public int M;
    public int realN;
    public int realM;
    String grouptype1;
    String grouptype2;
    IntMat MB = new IntMat();
    public boolean gathered = false; //true;
    int bigJ;
    int placeT, placeS, placeTm;

    // we're going to generally make the group be of the
    // form Gamma_0(N) intersect Gamma_1(M)
    // with N a multiple of M
    // we'll have to put it in this form, and get
    // back to original form by conjugation at the end
    // the matrix we'll conjugate by is called MB

    //constructor

    public RepList(String type_of_group1, String type_of_group2){
	reps[0] = new ConjClassRep(1,0,0,1);
	table[0][0][0]=1;
	
	// actually, group type is not really set hear at all, since
	//M and N not yet set; will be really set when makeup is run (below)
	grouptype1=type_of_group1;
	grouptype2=type_of_group2;
	//	grouptype=type_of_group();
	CuspNo = 1;  // these two lines are cheating for case N=1
	cusps[0][0]=1; 	cusps[0][2]=1; 

    }

    public String type_of_group(){
	// this function should take 
	// Gamma01?(N) intersect Gamma01?(M)
        // one idea was to put in a nice standard form
	// so, it will also change N and M to nicer values appropriately
	// The form we put things temporarily into is usually
	// Gamma_0(N) intersect Gamma_1(M), with N a multiple of
	// M.  This should be sorted out earlier so things are in this
        // however, currently this idea is abandonned.
	// hence this function is pretty irrelevant right now, 
	// and is not called.
	realN = N;
	realM = M;
	ConFrac cf;

	//	System.out.println("N,M = "+N+" "+M);
	// System.out.println("realN,readM = "+realN+" "+realM);
	if (M==1) return grouptype1;
	else if (N==1) 
	    {this.N = realM; 
	    //   System.out.println("N,M = "+N+" "+M);
	    return grouptype2.replace('M','N');}
	else if (grouptype2.replace('M','N').equals(grouptype1)){
	    cf = new ConFrac(N,M);
	    N = N*M/cf.gcf;
	    M = N;
	    return (grouptype1);
	}
	else { 
	    // this is all the other cases. 
	    // we'll keep track of a conjugation matrix
	    // MB which we'll have to apply at the end to
	    // get back to the correct form.
            // Note, MB might look to be changed by [0,1;-1,0]
	    // from what you think it should be - this is because
	    // although the group type says the subscript is lower,
	    // really the working is done as thought it was upper.
	    return "Gamma(N)";
	}

	

    }

    
    // method
    // length tells you the current length

    public int length(){
	int i=0;

	while(this.table[i][0][0]!=0) {++i;};
	  
	return i;
    }

    private void findcusps(){
	int C=0;
	int i,j;
	int len = length();	
	cusps[0][2]=0;
	int allTris[] = new int [len];
	
	int k;
	for (i=0;i<len;i++){

	    if (allTris[i]==0){
		allTris[i]=C+1;
		if (reps[i].c <0) {
		    cusps[C][0]=-reps[i].a;
		    cusps[C][1]=-reps[i].c;}
		else {
		cusps[C][0]=reps[i].a;
		cusps[C][1]=reps[i].c;}
		
		cusps[C][2]++;
		j=table[i][1][1];
		allTris[j]=C+1;
		while (j!=i){
		    j=table[j][1][1];
		    cusps[C][2]++;
		    allTris[j]=C+1;
		}
		C++;
	    }
	}
	CuspNo = C;
    }


    private void findelliptic(){
	int E=0;
	int i,j;
	int len = length();	
	int[] allTris = new int [len];
	int[][] ellpt = new int[len][3]; // possible ord 3 elliptic points
	int k;
	int E3=0;  // number of ord 3 elliptic points

	for (i=0;i<len;i++){

	    if (allTris[i]<2){
		allTris[i]++;         // start with next triangle
		// System.out.println("i= "+i);
		//if (reps[i].c <0) {
		//    ellpt[E][0]=-reps[i].a;
		//    ellpt[E][1]=-reps[i].c;}
		//else {
		//ellpt[E][0]=reps[i].a;
		//ellpt[E][1]=reps[i].c;}
		ellpt[E][2]++;

		j=table[i][1][1];   // translate by T
		allTris[j]++;
		// System.out.println("j= "+j);
		ellpt[E][2]++;

		j=table[j][3][1];   // translate by S

		if (j!=i){             // don't continue if we're back to i
		    allTris[j]++;     
		    allTris[j]++;    // this is becase if 
		    //               // we've been through in the T direction
		    //               // once, (which is what's coming up,
		    //               // we won't need to go though it again
		    //               // so the loop should never start with
		    //               // j again (and allTris[j] is tested
		    //               // on entry to loop at stage j.)
		    //	    System.out.println("j= "+j);
		    ellpt[E][2]++;
		    j=table[j][1][1];   // translate by T
		    allTris[j]++;
		    //System.out.println("j= "+j);
		    ellpt[E][2]++;
		    j=table[j][3][1];   // translate by S
		    allTris[j]++;
		    allTris[j]++;     // same reason as above  
		    //System.out.println("j= "+j);
		    ellpt[E][2]++;
		    j=table[j][1][1];   // translate by T
		    allTris[j]++;
		    //System.out.println("j= "+j);
		    ellpt[E][2]++;
		}
		else E3++;
		E++;

	    }
	}

	int E2=0;
	for(i=0;i<len;i++){
	    if (table[i][3][1]==i) E2++;
	    //	    System.out.println("el= "+ellpt[i][2]);
	}
	//for(i=0;i<len;i++){
	//    System.out.println("tr= "+allTris[i]);
	//}

	//System.out.println("ellptic: ");
	//System.out.println(" "+E3+" "+E2);

	genus = 1 + (len - 6*CuspNo -4*E3 - 3*E2)/12;

    }


    // make makes up the rep list

    public void makeup(int N, int M){
	this.M = M;
	this.N = N;

	// System.out.println();
	//	grouptype=type_of_group();
	//	 System.out.println("M="+this.M+"  N="+this.N+" gptp = "+grouptype);

	// the following is to get things ballanced about the middle a little
	//	if (N!=1 && grouptype1.equals("Gamma^0(N)")) reps[0] = new ConjClassRep(1,-N/2,0,1);
	// else if (grouptype1.equals("Gamma^0(M)")) reps[0] = new ConjClassRep(1,-M/2,0,1);
	dist[0]=0;
	//	this.N = N;
	N = this.N;
	M = this.M;
	next(N,M,0);
	ModN modN = new ModN(N);

	
	if ((grouptype.equals("Gamma_0(N)"))||(grouptype.equals("Gamma_1(N)"))
		||(grouptype.equals("Gamma(N)"))) {
	    int i;
	    ConjClassRep S  = new ConjClassRep(IntMat.S);
	    for (i=0;i<this.length();i++){
		reps[i]= new ConjClassRep(S.mult(reps[i]));
	    }
	}


	if ((grouptype.equals("Gamma(N)"))) {
	    int i,j,len2;
	    int k,m,B,C,Bb,Ma,Mb,Mc,Md;
	    int len = this.length();
	    ConjClassRep T  = new ConjClassRep(IntMat.T);
	    ConjClassRep MM  = new ConjClassRep(IntMat.Id);		
	    IntMat[] sp = new IntMat[5];
	    int[] p = new int[5];
	    int pn = 0;
	    boolean found6=false;
	    
	    // search for 5 special matrics:

	    boolean[] found = new boolean[5];
	    for (i=0;i<5;i++) found[i]=false;
	    sp[0] = new IntMat(1,0,0,1);
	    sp[1] = new IntMat(0,-1,1,-1);
	    sp[2] = new IntMat(0,-1,1,1);
	    sp[3] = new IntMat(1,0,2,1);
	    sp[4] = new IntMat(1,0,-2,1);

	    j=0;	    
	    while (j<len && !(found[0]&&found[1]&&found[2]&&
			     found[3]&&found[4]))
		{
		    found6=false;
		    i=0;
		    while(i<5 && !found6){
			if (reps[j].fracEqual(sp[i])){
			    found6=true;
			    found[i]=true;
			    p[i]=j;
			}
		    i++;
		    }
		    j++;
		}
	    
	    if (found[0]) {
		table[p[0]][2][2]=1;
		table[p[0]][1][2]=1;    
	    }
	    
	    if (found[1] && found[2]){
		table[p[1]][3][2]=1;
		table[p[2]][3][2]=1;
	    }

	    if (found[3] && found[4]){
		table[p[3]][2][2]=1;
		table[p[4]][1][2]=1;
	    }


	    for (j=0;j<len;j++){
		B = reps[j].mult(IntMat.T).div(reps[table[j][1][1]]).b;
		Bb = modN.mod(B); 
		table[j][1][1] = table[j][1][1] + Bb*len;
		
		B = reps[j].mult(IntMat.Tm).div(reps[table[j][2][1]]).b;
		Bb = modN.mod(B); 
		table[j][2][1] = table[j][2][1] + Bb*len;

		B = reps[j].mult(IntMat.S).div(reps[table[j][3][1]]).b;
		Bb = modN.mod(B); 
		table[j][3][1] = table[j][3][1] + Bb*len;

	    }
		
	    for (i=1;i<N;i++){
		MM  = new ConjClassRep(MM.mult(T));
		    for (j=0;j<len;j++){
			reps[j+len*i] = new ConjClassRep(MM.mult(reps[j]));
			table[j+len*i][0][0]=1;
			for (k=1;k<=3;k++){
			    for (m=0;m<=2;m++){
				if (m!=1)
				    table[j+len*i][k][m]=table[j][k][m];
				else {
				    C = table[j][k][1]/len;
				    B = table[j][k][1] - C*len;
				    table[j+len*i][k][1]=modN.mod(C + i)*len+B;
				}}
			}
		    }
	    }
	
	    // correct the stuff at either end:
       
	    if (found[0]){
		table[len*(N-1)+p[0]][1][2]=0;
		          table[p[0]][2][2]=0;}
	    if (found[1] && found[2]){
		table[len*(N-1)+p[1]][3][2]=0;
		          table[p[2]][3][2]=0;}
	    if (found[3] && found[4]){
		table[len*(N-1)+p[3]][2][2]=0;
		          table[p[4]][1][2]=0;}
	
	    
	}

     	int i;
	//		System.out.println("---");
	//for(i=0;i<this.length();i++){
	//    System.out.println(" "+table[i][0][0]+" "+table[i][1][1]+" "+table[i][2][1]+" "+table[i][3][1]);
	//}
		

	// correct the fact that we conjugated in the first place:
	int len = length();
	for (i=0;i<len;i++){
	    reps[i].conjugate(MB); 
	}

	findcusps();
	findelliptic();  
        
    }
    
    // i is the place we're working at. (ie, the 'next' thing just added)

    public void next(int N, int M, int i){
	int j=0,test;	

	ConjClassRep A  = new ConjClassRep();
	ConjClassRep T  = new ConjClassRep(IntMat.T);
	ConjClassRep Tm  = new ConjClassRep(IntMat.Tm);
	ConjClassRep S  = new ConjClassRep(IntMat.S);

	placeT=0; placeS=0; placeTm=0;
	
	
	int l;
	int current_i;
	current_i = i;
	if (!gathered){
	for (l=0;l<i;l++){
	    if (table[l][1][0]*table[l][2][0]*table[l][3][0]==0){
		if (dist[l]<dist[current_i]) current_i = l;
	    }}
	i=current_i;
	}
	
	A = reps[i];
	// System.out.println(i+" : "+A.a+" "+A.b+" "+A.c+" "+A.d);
	
	if (table[i][1][0]==0){
	ConjClassRep AT = new ConjClassRep(A.mult(T));
	// System.out.println("T direction");
	if (!alreadyInTheList(i,j,1,2,AT)) {
	    j = bigJ;

	    // for the MinDist thing below we need the next line:
	    placeT=i; placeS=i;
	    findnextdirections(AT,T,S,placeT,placeS);
	    
	    if (gathered) placeS=i;
	    int MinDist = Math.min(Math.min(dist[placeT],dist[placeS]),dist[i]);
	    if      (dist[i]     ==MinDist)  stuff(j,i,T,1,2);	    
	    else if (dist[placeT]==MinDist)  stuff(j,placeT,Tm,2,1);
	    else stuff(j,placeS,S,3,3);

	    if ((reps[j].c==0) && (grouptype.equals("Gamma^0(N)"))) dist[j]=0;
	    table[j][0][0]=1;
	    // System.out.println("j="+j);
		next(N,M,j);
	    }
	    else if (gathered) {
		// if already in the list, then we should have completed a 'loop',
		// maybe can alter connections to get smaller distances:
		j = bigJ;
		placeTm=i; placeT=i;
		findnextdirections(A,Tm,T,placeTm,placeT);
		//	System.out.println("placeT = "+placeTm);
		// System.out.println("placeTm = "+placeT);
		if (dist[placeT]<dist[placeTm]){
		    stuff(i,placeT,Tm,2,1);
		}
	    }
	}
	
	if (table[i][2][0]==0){
	    ConjClassRep ATm = new ConjClassRep(A.mult(Tm));
	    // System.out.println("Tm direction");
	    if (!alreadyInTheList(i,j,2,1,ATm)) {
		j = bigJ;

		placeTm=i; placeS=i;
		findnextdirections(ATm,Tm,S,placeTm,placeS);
		if (gathered) placeS=i;
		
		int MinDist = Math.min(Math.min(dist[placeTm],dist[placeS]),dist[i]);
		if      (dist[i]      ==MinDist)  stuff(j,i,Tm,2,1);	    
		else if (dist[placeTm]==MinDist)  stuff(j,placeTm,T,1,2);
		else                              stuff(j,placeS,S,3,3);

		if ((reps[j].c==0) && (grouptype.equals("Gamma^0(N)"))) dist[j]=0;
		table[j][0][0]=1;
		// System.out.println("j="+j);
		next(N,M,j);
		}
	    else if (gathered) {
		// if already in the list, then we should have completed a 'loop',
		// maybe can alter connections to get smaller distances:
		j = bigJ;
		placeTm=i; placeT=i;
		findnextdirections(A,Tm,T,placeTm,placeT);
		// System.out.println("placeTm = "+placeTm);
		// System.out.println("placeT = "+placeT);
		if (dist[placeTm]<dist[placeT]){
		    stuff(i,placeTm,T,1,2);
		}
	    }
	}
	
	if (table[i][3][0]==0){
	    ConjClassRep AS = new ConjClassRep(A.mult(S));	
	    // System.out.println("S direction");
	    if (!alreadyInTheList(i,j,3,3,AS)) {
		j = bigJ;

		placeTm=i; placeT=i;
		
		findnextdirections(AS,T,Tm,placeT,placeTm);
		
		int MinDist = Math.min(Math.min(dist[placeTm],dist[placeT]),dist[i]);
		if      (dist[i]      ==MinDist)  stuff(j,i,S,3,3);	    
		else if (dist[placeTm]==MinDist)  stuff(j,placeTm,T,1,2);
		else                              stuff(j,placeT,Tm,2,1);

		if ((reps[j].c==0) && (grouptype.equals("Gamma^0(N)"))) dist[j]=0;
		table[j][0][0]=1;
		// System.out.println("j="+j);
		next(N,M,j);
	    }
	}
		
	l=0;
	boolean more=false;
	while(!more && (l<this.length())){
	    if (table[l][1][0]*table[l][2][0]*table[l][3][0]==0){
		more = true;
	    }
	    l++;
	}
	if (more) next(N,M,l-1);

    }
    
    public boolean alreadyInTheList(int i, int j, int d1, int d2, ConjClassRep Mat){
	// this checks whether the matrix Mat is already in the list;
	// if it is, it joins up the edges to the ith edge correctly

	int m=0;
	boolean found=false;
	while (m<this.length() && !found) {
	    if (Mat.equiv(grouptype1,grouptype2,N,M,reps[m])) 
		{found=true;
		connect(i,m,d1,d2,Mat);
		}
	    m++; 
	}
	bigJ = m;
	// System.out.println("found="+found);
	return found;
    }

    private void findnextdirections(ConjClassRep AD,
				    ConjClassRep Mat1,
				    ConjClassRep Mat2,
				    int place1,int place2){
	ConjClassRep AD1 =  new ConjClassRep(AD.mult(Mat1));
	ConjClassRep AD2 =  new ConjClassRep(AD.mult(Mat2));
	int k = 0;
	boolean next1 = false; 
	boolean next2 = false;
	while (((!next1)||(!next2))&& (k<this.length())){
	    if (AD1.equiv(grouptype1,grouptype2,N,M,reps[k]))
		{next1=true; place1 = k;}
	    if (AD2.equiv(grouptype1,grouptype2,N,M,reps[k]))
		{next2=true; place2 = k;}
	    k++;
	}
    }


    private void connect(int i, int j, int d1, int d2, ConjClassRep Mat){
	table[i][d1][0] = 1;
	table[i][d1][1] = j;
	table[j][d2][0] = 1;
	table[j][d2][1] = i;
	if ((Mat.equal(reps[j])) || (Mat.equal(reps[j].negate()))){
	    table[i][d1][2] = 1;
	    table[j][d2][2] = 1;
	}
    }
    
    private void stuff(int j,int position,ConjClassRep Mat,int d1,int d2){
	reps[j] = new ConjClassRep(reps[position].mult(Mat));
	dist[j] = dist[position]+1;		    
	table[position][d1][0] = 1;
	table[position][d1][1] = j;
	table[position][d1][2] = 1;
	table[j][d2][0] = 1;
	table[j][d2][1] = position;
	table[j][d2][2] = 1;
    }
    
}










