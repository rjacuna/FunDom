// this version is only for PSL subgroups
//SL2 := [ ];
//GL2 := [ ];
//load "pre.m";


//////////////////////////////////////////////
// a format for congruence subgroups of sl2(Z)

CongSubGrp := recformat<generators:SeqEnum, 
                      level:Integers(), 
                      genus:Integers(),
                      index:Integers(),
                      contains_minus_one:BoolElt,
                      subgroups:SeqEnum,
                      supergroups:SeqEnum,
                      name:MonStgElt,        // (level)(label)(genus)
                      label:MonStgElt,
                      order:Integers(),
                      GLZConj:Integers(),
                      matgens:SeqEnum,
                      cusps:SeqEnum,
                      c2:SeqEnum,
                      c3:SeqEnum,
                      gc:SeqEnum,            // galois conjugates
                      projections:SeqEnum,
                      special_name:SeqEnum,
                      special_name_html:SeqEnum,
                      direct_subgroups:SeqEnum,
                      direct_supergroups:SeqEnum,
                      length:Integers(),
                      treename:SeqEnum,
                      treesupergroups:SeqEnum
                     >;

// all variables called grp, grp1, grp2... are of type CongSubGrp
// variables called grps, Lgrps, grpL... are lists (SeqEnum) with
// elements of type CongSubGrp
//////////////////////////////////////////////

//////////////////////////////////
//
//  S = [[0,1],[-1,0]]
//  T = [[1,1],[0,1]]
//
/////////////////////////////////////////////////////////////////////

mu := function(n)

  fac := Factorization(n);
  ind := n^3;
  for i:= 1 to #fac do
    ind := ind * (1-1/(fac[i][1])^2);
  end for;
  return ind;
end function;

///////////////////////////////////////////////////////////////////////
// principal congruence subgroups in permutation representation
///////////////////////////////////////////////////////////////////////

sl2 := function(m)

  if IsDefined(SL2,m) then
    return SL2[m];
  end if;

  if m eq 1 then
    return sub<Sym(1) | [1],[1]>;
  end if;


//  print "computing SL( 2,",m,")";

  factL := Factorization(m);

  M := 0;
  S := [ ];
  T := [ ];
  for fact in factL do
    s,t := sl2_generators(fact[1],fact[2]); 
    ss := [ u+M : u in s ];
    tt := [ u+M : u in t ];
    S cat:= ss;
    T cat:= tt;
    M := M + #s;
  end for;

  G := Sym(M);
  gens:=[G|S,T];
  H:=sub<G | gens>;
  H:=DegreeReduction(H);

  return(H);  

end function;

///////////////////////////////////////////////////////////////////

gl2:=function(m);

  if IsDefined(GL2,m) then
//    print "+++ gl2(",m,") known";
    return GL2[m][1],GL2[m][2];
  end if;
//  print "--- gl2(",m,") computing";
  g1:=nsl2(m);
  gens:=gl2_extra_generators(m);
  M:=Degree(g1);
  H:=sub<Sym(M)|g1,gens>;
  D:=sub<Sym(M)|gens>;
  return H,D;
end function;


///////////////////////////////////////////////////////////////////
// conversion between permutation representation and CongSubGrp
// format
///////////////////////////////////////////////////////////////////

fgrouptolist := function(G,S)
////////////////////////////
// return a list containing the generators of the group in list form 
////////////////////////////
//  print "new grouptolist ---";
  free := FreeGroup(#Generators(G));
  s := free.1;
  t := free.2;
  phi := hom<G->free|[s,t]>;
  gen := Generators(S);
  L := [ ElementToSequence(g@phi) : g in gen];
    //print "            --- done";
  return L;
end function;

/////////////////////////////////////////////////////////////////

fgamma := function(G1,G2)
/////////////////////////
// G1: group
// G2: group
// where G1 and G2 have the 'same' two generators and G1 contains G2
// returns a G2 as a subgroup of G1
// for example:  G1 = sl2(p^(k+1)), G2 = sl2(p^k)
/////////////////////////
  s := G2.1; 
  t := G2.2;
  phi := hom< G1-> G2 | [s, t] >;
  gamma := Kernel(phi);
  return gamma;
end function;


///////////////////////////////////////////////////////////////////

flisttoelt := function(G,L)

  elt := G.0;
  for l in L do
    elt := elt*G.l;
  end for;

  return elt;
end function;


flisttogroup := function(G, L)

  return sub<G | [flisttoelt(G,l) : l in L]>;

end function;


csg_perm_group := function(grp)
///////////////////////////////
// construct a permutation representation of the group grp
///////////////////////////////
  princ_group := sl2(grp`level);
  return flisttogroup(princ_group, grp`generators);

end function;

//////////////////////////////////////////////////////////////////////
// genus and level - permutation representation
//////////////////////////////////////////////////////////////////////

fgenus := function(G, S)
////////////////////////
// G: principal congruence group in permutation representation
// S: a subgroup of G
////////////////////////
  CS,im:=CosetAction(G,S);
  deg:=Degree(im);
  h:=CS;

  X:=G.1;
  Y:=G.2;

  a:=[h(X),h(Y),h(X*Y)];

  ng:=3;

  cs:=[CycleStructure(a[i]) : i in [1..ng]];


  ans:=-2*deg + 2;
  for j:=1 to ng do
    for i:=1 to #cs[j] do
    ans:=ans + (cs[j][i][1]-1)*cs[j][i][2];
    end for;
  end for;
  return ans div 2;

end function;


flevel := function(G,S)
///////////////////////
// G: principal congruence group in permutation representation
// S: a subgroup of G
///////////////////////
  T := G.2; 
// print "level ---";
  c := CosetTable(G,S);
  p := CosetTableToRepresentation(G,c);
  lev :=  Order(p(T));
// print "      --- is",lev;
  return lev;
end function;

/////////////////////////////////////////////////////////////////////

csg_short := function(grp)
//////////////////////////
// returns the same group with shorter generators
//////////////////////////
  if grp`index eq 1 then
    return grp;
  else 
    permgrp := csg_perm_group(grp);
    grp`generators := fgrouptolist(sl2(grp`level),permgrp);
  end if;

  return grp;
end function;


//////////////////////////////////////////////////////////////////////

csg_GLZ_conj:=function(grp);

  s:=grp;
  s`generators:=[ [ -i : i in ll] : ll in grp`generators];
  return s;
end function;

///////////////////////////////////////////////////////////////////////

csg_is_conjugate := function(grp1, grp2)
////////////////////////////////////////
// return true if grp1 and grp2 are conjugate subgroups of
// sl2(grp1`level = grp2`level)
////////////////////////////////////////
// print p,grp1,gen1,lev1,ind1,
//       grp2,gen2,lev2,ind2,
//       Lgam;

  if grp1`index eq 1 and grp2`index eq 1 then 
    return true;
  elif grp1`genus ne grp2`genus then 
    return false;
  elif grp1`level ne grp2`level then 
    return false;
  elif grp1`index ne grp2`index then 
    return false; 
  elif grp1`generators eq grp2`generators then 
    return true; 
  end if;

// print "conjugate ?";
 
  lev := grp2`level; 
  G1 := csg_perm_group(grp1);
  G2 := csg_perm_group(grp2);

  isconjugate := IsConjugate(sl2(lev),G1,G2);
// print "          ? ",isconjugate; 
  return isconjugate;
end function;


csg_in_list := function(grp1, Lgrps)
  for i := 1 to #Lgrps do
    if csg_is_conjugate(grp1, Lgrps[i]) then
      return true,i;
    end if;
  end for;
  return false, -1;
 
end function;


csg_GLZ_in_list := function(grp1, Lgrps)
  grp1c := csg_GLZ_conj(grp1);
  for i := 1 to #Lgrps do
    if csg_is_conjugate(grp1c, Lgrps[i]) then
      return true,i;
    end if;
  end for;
  return false,-1;
end function;


csg_conjugate_in_list := function(grp, Lgrps)
// tests for PSL and PGL conjugacy

  grpc := csg_GLZ_conj(grp);
  for i := 1 to #Lgrps do
    if csg_is_conjugate(grp, Lgrps[i]) then
      return true,i;
    end if;
    if csg_is_conjugate(grpc, Lgrps[i]) then
      return true,i;
    end if;
  end for;
  return false,-1;
end function;


csg_is_GLZ_subgroup := function(grp1, grp2)
///////////////////////////////////////
// returns true if grp2 is a subgroup of grp1
// up to GLZ and PSL conjugacy
///////////////////////////////////////

  if grp1`index eq 1 then return true; end if;
  if grp2`index eq 1 and grp1`index ne 1 then return false; end if;

  if grp1`genus gt grp2`genus then return false; end if;
  if grp1`level gt grp2`level then return false; end if;
  if grp1`index gt grp2`index then return false; end if;
  if grp2`level mod grp1`level ne 0 then return false; end if;
  if grp2`index mod grp1`index ne 0 then return false; end if;
//print grp1;print grp2;print "subgroup ?";
  G2 := csg_perm_group(grp2);
  gam := sl2(grp2`level);
  G1 := sub<gam |  fgamma(gam,sl2(grp1`level)),
                   flisttogroup(gam, grp1`generators)>;
  G2c:= sub<gam |  fgamma(gam,sl2(grp1`level)),
                   flisttogroup(gam, [[-i:i in ll]:ll in grp2`generators])>;

  R := RightTransversal(gam, G1);
  for r in R do
    G1c1 := G1^(r^-1);
    G1c2 := G1^(r);
    if G2 subset G1c1 or G2 subset G1c2 then
      return true;
    elif G2c subset G1c1 or G2c subset G1c2 then
      return true;
    end if;
  end for;

  return false;

end function;




csg_list_add_conjugate := procedure(~Lgrps,grp,~added,searchsupergroups)
///////////////////////////////////////////////////////////////
// if no conjugate (this includes GLZ conjugates) of grp is in
// Lgrps, add grp to Lgrps, otherwise transfer the treesupergroup
// information.  added is true if grp has been added.
///////////////////////////////////////////////////////////////

  con_in_list, pos := csg_conjugate_in_list(grp,Lgrps);
  // this test includes GLZ conjugation
  if con_in_list then
    Lgrps[pos]`treesupergroups cat:= grp`treesupergroups;
    Lgrps[pos]`treename cat:= grp`treename;
    if grp`GLZConj eq 2 then Lgrps[pos]`GLZConj:=2; end if;
    added := false;
//    print "conjugate excluded";
  elif searchsupergroups then
  // grp could hav other supergroups than those given by the calculation,
  // look for those.
print "searchsupergroups";
//    knownsupergroups := [ ];
    grp := csg_short(grp);
    for i:=#Lgrps to 1 by -1 do
      g := Lgrps[i];
      factL :=  Factorization(grp`level);
      p := factL[#factL][1];
      if g`level mod p ne 0 and g`index ne 1 then
        if not g`treename[1] in grp`treesupergroups then
//          if not g`treename[1] in knownsupergroups then
            if csg_is_GLZ_subgroup(g, grp) then
              Append(~grp`treesupergroups,g`treename[1]);
//              knownsupergroups cat:= g`treesupergroups;
print "supergroup",g`treename," added to group",grp`treename;    
            end if;
//          end if;
        end if;
      end if;
    end for;
    if not csg_is_conjugate(csg_GLZ_conj(grp),grp) then
      grp`GLZConj:=2;
    end if;
    Append(~Lgrps, grp);
    added := true;
  else
    if not csg_is_conjugate(csg_GLZ_conj(grp),grp) then
      grp`GLZConj:=2;
    end if;
    Append(~Lgrps, csg_short(grp));
    added := true;
  end if;
end procedure;


//////////////////////////////////////////////////////////////////////
// computation of lists of congruence subgroups
//
//////////////////////////////////////////////////////////////////////

fsubs := function(G,S,maxgen,Streename)
// return all maximal subgroups of S in G that have genus <= maxgen
// print "subs -- start";
  Lgrp := [ ];
  minusone := G.1^2;

  M := MaximalSubgroups(S);
// print " -- #maxsubgroups ",#M;
  for m := 1 to #M do
    if minusone in M[m]`subgroup then     
      ind := Index(G,M[m]`subgroup);
// print "     -- ind ",ind;
      if ind lt (1+maxgen)*128 then
        gen := fgenus(G,M[m]`subgroup);
// print "     -- gen ", gen;
        if gen le maxgen then
          if #Streename eq 1 then
            super := [[0]];
          else 
            super := [Streename];
          end if;
          lev := flevel(G,M[m]`subgroup);
          newgrp := rec<CongSubGrp|
                         generators := fgrouptolist(G,M[m]`subgroup),
                         level := lev,
                         index := ind,
                         genus := gen,
                         contains_minus_one := true,
                         treesupergroups := super,
                         treename := [Append(Streename,m)],
                         GLZConj:=1
                        >;

          csg_list_add_conjugate(~Lgrp,newgrp,~added, false);

        end if;
      end if;
    end if;
  end for;
// print "     -- level ",Llev;
// print "     -- index ",Lind;
  return Lgrp;
end function;


forward csg_p_subgroups_sub;
csg_p_subgroups_sub := procedure(p,k,maxgen,Sgen, ~Lgrps, Streename)

// print "pSubgroupsUpTo: ",p^k;
   if k eq 1 then
     if p le 5 then
       grps := fsubs( sl2(p^2),  sl2(p^2),maxgen, Streename);
     else
       grps := fsubs( sl2(p),  sl2(p),maxgen, Streename);
     end if;
   else
      grps := fsubs( sl2(p^k), 
                     sub<sl2(p^k) | 
                         fgamma(sl2(p^k),sl2(p^(k-1))), 
                         flisttogroup( sl2(p^k),Sgen)>,
                     maxgen, Streename);
   end if;
  
   for grp in grps do

     csg_list_add_conjugate(~Lgrps,grp,~added,false);

     if added then
       lev:= grp`level;
       levexp := Valuation(lev, p);
// print "groups - ",lev;
       SSgen := grp`generators;
       csg_p_subgroups_sub(p,levexp+1,maxgen,SSgen,~Lgrps, grp`treename[1]);
     end if;
   end for;
end procedure;


csg_p_to := function(p,maxgen,Streename)
//////////////////////////////
// returns all groups of level p^n up to genus maxgen
//////////////////////////////
    Lgrps := [ ]; 
    csg_p_subgroups_sub(p,1,maxgen,[ ], ~Lgrps,Streename);
  
  return Lgrps;
end function;


forward csg_subgroups_to;
csg_subgroups_to := procedure(grp, maxlevel, maxgenus, ~Lgrps)
/////////////////////////////////////////////////////
// returns all subgroups of grp up to level maxlevel and up to genus maxgenus
/////////////////////////////////////////////////////

 if maxlevel mod grp`level ne 0 then
   Lgrps cat:= [ ];     
 elif (1+maxgenus)*64 lt grp`index then
   Lgrps cat:= [ ];
 else
   lev := grp`level;
//   print "csg_subgroups_to:      input level:",lev,"  search level: ",maxlevel;
   grps := fsubs( sl2(maxlevel), 
                  sub<sl2(maxlevel) | 
                         fgamma(sl2(maxlevel),sl2(lev)), 
                         flisttogroup( sl2(maxlevel),grp`generators)>,
                  maxgenus, grp`treename[1]);

   for grp in grps do
// print "  found level:", grp`level;

     csg_list_add_conjugate(~Lgrps,grp,~added,false);

     if added then
       csg_subgroups_to(grp, maxlevel, maxgenus, ~Lgrps);
     end if;
   end for;
end if;  

end procedure;
                          

csg_list_to := function(grps, maxlevel, maxgenus)
/////////////////////////////////////////////////
// returns all subgroups up to level maxlevel and up to genus maxgenus
// of the groups in the list grps
/////////////////////////////////////////////////
  Lgrps := [ ];
  for grp in grps do
    csg_subgroups_to(grp, maxlevel, maxgenus, ~Lgrps);
  end for;

  return Lgrps;

end function;


maximal_levels := function(genus)
/////////////////////////////////
// 
/////////////////////////////////
  prime_bound := 12*genus+13;
  levels := significant_levels(level_bound(genus), prime_bound);
  lower_bound := Ceiling(level_bound(genus)/2);
  maxlevels := [u : u in levels | u ge lower_bound];

  return maxlevels;

end function;


csg_to_level := function(maxgenus, level)
//////////////////////
// returns a list of all congruence subgroups of level up to level and 
// genus up to maxgenus
//////////////////////

  factL := Factorization(level);
  if #factL eq 1 then
    grps := csg_p_to(factL[1][1],maxgenus,[level]);
  elif factL[#factL][1] gt 5 then
    // in this case we do not get the complete supergroup structure
    baslevel := factL[#factL][1];
    basegrps := fsubs(sl2(baslevel),sl2(baslevel),maxgenus,[level]);
    grps := basegrps cat csg_list_to(basegrps, level, maxgenus); 
  else
    basegrps := fsubs(sl2(level),sl2(level),maxgenus,[level]);
    grps := basegrps cat csg_list_to(basegrps, level, maxgenus);
  end if;
  return grps;

end function;



csg_list_expand := procedure(~Lgrps, startlevel, maxgenus, onlycomposite)
//////////////////////
// returns a list of all congruence subgroups of genus up to maxgenus
//////////////////////

  if not assigned Lgrps then Lgrps := []; end if;

  if #Lgrps eq 0 then
    Lgrps := [ rec<CongSubGrp|
                         generators := [],
                         level := 1,
                         index := 1,
                         genus := 0,
                         contains_minus_one := true,
                         name := "1A0",
                         label := "A",
                         length:=1,
                         order:=1,
                         GLZConj:=1,
                         c2:=[1],
                         c3:=[1],
                         matgens:=[],
                         gc:=[1],
                         cusps:=[1],
                         treename := [[0]],
                         treesupergroups := []
                         >
             ];
  end if;
  
  maxlevels := maximal_levels(maxgenus);

  for level in maxlevels do
  if level ge startlevel and not onlycomposite then
    factL:= Factorization(level);
    grps := [];
    if #factL eq 1 then
print "LEVEL:",level;
      grps := csg_p_to(factL[1][1],maxgenus,[level]);
    elif factL[#factL][1] le 5 then
print "LEVEL:",level;
      basegrps := fsubs(sl2(level),sl2(level),maxgenus,[level]);
      grps := basegrps cat csg_list_to(basegrps, level, maxgenus);
    end if;
 
    for grp in grps do
      csg_list_add_conjugate(~Lgrps,grp,~added,false);
    end for;
      
    //csg_list_clean_treesupergroups(~Lgrps);
      
  end if;   
  end for;
  for level in maxlevels do
  if level ge startlevel then
    factL:= Factorization(level);
    grps :=[];
    if factL[#factL][1] gt 5 and #factL gt 1 then
    print "LEVEL:",level;
    // in this case we do not get the complete supergroup structure
      baslevel := factL[#factL][1];
      basegrps := fsubs(sl2(baslevel),sl2(baslevel),maxgenus,[level]);
      grps := basegrps cat csg_list_to(basegrps, level, maxgenus);

      for grp in grps do
        csg_list_add_conjugate(~Lgrps,grp,~added,true);
      end for;
    end if;
    //csg_list_clean_treesupergroups(~Lgrps);
  end if;
  end for;
 
end procedure;


csg := function(maxgenus)
//////////////////////
// returns a list of all congruence subgroups of genus up to maxgenus
//////////////////////
  Lgrps := [ rec<CongSubGrp|
                         generators := [],
                         level := 1,
                         index := 1,
                         genus := 0,
                         contains_minus_one := true,
                         name := "1A0",
                         label := "A",
                         length:=1,
                         order:=1,
                         GLZConj:=1,
                         c2:=[1],
                         c3:=[1],
                         matgens:=[],
                         gc:=[1],
                         cusps:=[1],
                         treename := [[0]],
                         treesupergroups := []
                         >
 ];
  maxlevels := maximal_levels(maxgenus);
  for level in maxlevels do
    factL:= Factorization(level);
    grps := [];

    if #factL eq 1 then
print "LEVEL:",level;
      grps := csg_p_to(factL[1][1],maxgenus,[level]);
      for grp in grps do
        csg_list_add_conjugate(~Lgrps,grp,~added,false);
      end for;
 
    elif factL[#factL][1] le 5 then
print "LEVEL:",level;
      basegrps := fsubs(sl2(level),sl2(level),maxgenus,[level]);
      grps := basegrps cat csg_list_to(basegrps, level, maxgenus);
      for grp in grps do
        csg_list_add_conjugate(~Lgrps,grp,~added,false);
      end for;
    end if;
 
     
    //csg_list_clean_treesupergroups(~Lgrps);
      
  end for;
  for level in maxlevels do
    factL:= Factorization(level);
    grps :=[];
    if factL[#factL][1] gt 5 and #factL gt 1 then
print "LEVEL:",level;
    // in this case we do not get the complete supergroup structure
      baslevel := factL[#factL][1];
      basegrps := fsubs(sl2(baslevel),sl2(baslevel),maxgenus,[level]);
      grps := basegrps cat csg_list_to(basegrps, level, maxgenus);

      for grp in grps do
        csg_list_add_conjugate(~Lgrps,grp,~added,true);
      end for;
    end if;

    //csg_list_clean_treesupergroups(~Lgrps);
  end for;

//  csg_list_remove_conjugates(~Lgrps);  
  return Lgrps;

end function;

////////////////////////////////////////////////////////////////////////////










































///////////////////////////////////////////////////


///////////////////////////// JUNK


csg_to := function(maxlevel, maxgenus)
//////////////////////////////////////
// returns a list of all congruence subgroups with level up to maxlevel
// and genus up to maxgenus
//////////////////////////////////////
//  factL := Factorization(maxlevel);
//  p := factL[1][1];
//  if p gt 5 then
//    grps := fsubs(sl2(p),sl2(p),maxgenus,[maxlevel]);  
//  else
    grps := fsubs(sl2(maxlevel),sl2(maxlevel),maxgenus,[maxlevel]);
//  end if;

    Lgrps := csg_list_to(grps, maxlevel, maxgenus);

  return grps cat Lgrps; 

end function;

 

csg_list_by_treename := function(grps, name)

  ret := [ ];

  for i := 1 to #grps do
    if name in grps[i]`treename then
      Append(~ret,i);
    end if;
  end for;
  return ret;

end function;

csg_list_clean_treesupergroups := procedure(~grps)

  for i := 1 to #grps do
    if #csg_list_by_treename(grps,grps[i]`treename[1]) ne 1 then
      error "two groups:",csg_list_by_treename(grps,grps[i]`treename),   " with the same name",grps[i]`treename;
    end if;
    // remove supergroups with unknown names
    for sup in grps[i]`treesupergroups do
      if #csg_list_by_treename(grps, sup) eq 0 then
        Exclude(~grps[i]`treesupergroups,sup);
        print sup, "unknown, excluded";
      end if;
      grps[i]`treesupergroups := Setseq(Seqset(grps[i]`treesupergroups));
    end for;
  end for;
end procedure;


csg_GLZ_in_list := function(grp1, Lgrps)
  for i := 1 to #Lgrps do
    if csg_is_conjugate(grp1,csg_GLZ_conj(Lgrps[i])) then
      return true,i;
    end if;
  end for;
  return false,-1;
end function;





csg_list_remove_conjugates  := procedure(~Lgrps)
////////////////////////////////////////////////
// leave only one representative of each conjugacy class in the list
// Lgrps
////////////////////////////////////////////////
    old := #Lgrps;
    print "removing conjugates ...";
    newgrps := [ ];

    for i := 1 to #Lgrps do
      con, pos := csg_in_list(Lgrps[i], newgrps);
      if not con then
        Append(~newgrps, csg_short(Lgrps[i]));
      else
        newgrps[pos]`treesupergroups cat:= Lgrps[i]`treesupergroups;
        newgrps[pos]`treename cat:= Lgrps[i]`treename;
      end if;     
    end for;     
    Lgrps := newgrps;
    print "removed ",old-#Lgrps," of ",old;
 
end procedure;





csg_from := function(maxgenus, startlevel)
//////////////////////
// returns a list of all congruence subgroups of genus up to maxgenus
//////////////////////
  Lgrps := [ ];
  maxlevels := [Integers()| x : x in maximal_levels(maxgenus)|x ge startlevel];
  for level in maxlevels do
    factL := Factorization(level);
    if #factL eq 1 then
      grps := csg_p_to(factL[1][1],maxgenus);
    else
      grps := csg_to(level, maxgenus);
    end if;
    Lgrps cat:= grps;
  end for;
  csg_list_remove_conjugates(~Lgrps);  
  return Lgrps;

end function;



