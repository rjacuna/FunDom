
load "func.m";


/////////////////////////////////////////////////////////////////////
// selecting from a list of groups
/////////////////////////////////////////////////////////////////////

csg_list_by_genus := function(grps, gen)
////////////////////////////////////////
// returns a list of all groups in the list grps that have genus gen
////////////////////////////////////////
  Lgrps := [ ];
  for grp in grps do
    if grp`genus eq gen then
      Append(~Lgrps,grp);
    end if;
  end for;

  return Lgrps;
end function;


csg_list_by_minus_one := function(grps, minus_one)
////////////////////////////////////////
// returns a list of all groups in the list grps that
// contain -1 (minus_one = true) or
// do not contain -1 ((minus_one = false)
////////////////////////////////////////
  Lgrps := [ ];
  for grp in grps do
    if grp`contains_minus_one eq minus_one then
      Append(~Lgrps,grp);
    end if;
  end for;

  return Lgrps;
end function;


csg_list_by_index := function(grps, ind)
////////////////////////////////////////
// returns a list of all groups in the list grps that have index ind
////////////////////////////////////////
  Lgrps := [ ];
  for grp in grps do
    if grp`index eq ind then
      Append(~Lgrps,grp);
    end if;
  end for;

  return Lgrps;
end function;


csg_list_by_level := function(grps, lev)
////////////////////////////////////////
// returns a list of all groups in the list grps that have level lev
////////////////////////////////////////
  Lgrps := [ ];
  for grp in grps do
    if grp`level eq lev then
      Append(~Lgrps,grp);
    end if;
  end for;

  return Lgrps;
end function;



csg_list_by_levels := function(grps, lev, num)
////////////////////////////////////////
// returns a list of all groups G in the list grps 
// with lev le G`level lt lev+num
////////////////////////////////////////
  Lgrps := [ ];
  for grp in grps do
    if lev le grp`level and grp`level lt lev+num then
      Append(~Lgrps,grp);
    end if;
  end for;

  return Lgrps;
end function;



csg_list_with_special_name := function(grps)

  L := [];
  for g in grps do
    if g`special_name ne [] then
      Append(~L,g);
    end if;
  end for;
  return L;
end function;


csg_list_identical_groups := function(grps)
///////////////////////////////////////////
// grps must be sorted
///////////////////////////////////////////  

  L := [];
  for i:=1 to #grps-1 do
    if csg_compare(grps[i],grps[i+1]) eq 0 then
      Append(~L,grps[i]);
    end if;
  return L;
end for;
end function;



csg_list_by_name := function(grps, name)

  for g in grps do
    if g`name eq name then
      return g;
    end if;
  end for;
  return false;
end function;


///////////////////////////////////////////////////////////////////

csg_list_remove_GLZ_conjugates  := procedure(~Lgrps)

    old := #Lgrps;
    newgrps := [ ];

    for i := 1 to #Lgrps do
    con,j:=csg_GLZ_in_list(Lgrps[i], newgrps);
      if not con then
        Append(~newgrps, Lgrps[i]);
        newgrps[#newgrps]`GLZConj:=1;
      else
        newgrps[j]`GLZConj:=2;
      end if;
    end for;
    Lgrps := newgrps;
end procedure;



////////////////////////////////////////////////////////////////////
// improvement of groups and lists
////////////////////////////////////////////////////////////////////




csg_list_treename_pos := function(grps, name)

  for i := 1 to #grps do
    if name in grps[i]`treename then
      return i;
    end if;
  end for;
  return -1;

end function;


csg_list_clean_tree := procedure(~grps)

  for g := 1 to #grps do
    firstname := grps[g]`treename[1];
    names := grps[g]`treename;
    for i := 1 to #grps do
      for n := 2 to #names do
        if names[n] in grps[i]`treesupergroups then
          if not firstname in grps[i]`treesupergroups then
            Append(~grps[i]`treesupergroups, firstname);
          end if;
          Exclude(~grps[i]`treesupergroups,names[n]);
        end if;
      end for;
    end for;
    grps[g]`treename := [firstname];
  end for;
end procedure;








csg_list_tree_to_subgroups := procedure(~grps)

  Sort(~grps, csg_compare);

  for i := 1 to #grps do
    grps[i]`subgroups := [];
    grps[i]`supergroups := [];
  end for;

//  Sort(~grps, csg_compare);
  for g := 1 to #grps do
    for s in grps[g]`treesupergroups do
      pos := csg_list_treename_pos(grps,s);
      if not pos in grps[g]`supergroups then
        Append(~grps[g]`supergroups,pos);
      end if;
      if not g in grps[pos]`subgroups then
        Append(~grps[pos]`subgroups,g);
      end if;
    end for;
print g;
  end for;

  for i := 1 to #grps do
    Sort(~grps[i]`subgroups);
    Sort(~grps[i]`supergroups);
  end for;

end procedure;


csg_list_direct_subgroups := procedure(~Lgrps)

  if not assigned Lgrps[1]`subgroups then
    csg_list_tree_to_subgroups(~Lgrps);
  end if;
  len := #Lgrps;
  for i := 1 to len do
    subgroups := Lgrps[i]`subgroups;
    supergroups := Lgrps[i]`supergroups;
    Lgrps[i]`direct_subgroups := Lgrps[i]`subgroups;
    Lgrps[i]`direct_supergroups := Lgrps[i]`supergroups;
    for j in subgroups do
      subsubgroups := Lgrps[j]`subgroups;
      for k in subsubgroups do
        Exclude(~Lgrps[i]`direct_subgroups,k);
      end for;
    end for;
    for j in supergroups do
      supersupergroups := Lgrps[j]`supergroups;
      for k in supersupergroups do
        Exclude(~Lgrps[i]`direct_supergroups,k);
      end for;
    end for;
    Sort(~Lgrps[i]`direct_supergroups);
    Sort(~Lgrps[i]`direct_subgroups);
  end for;
end procedure;


csg_list_sort := procedure(~grps)
///////////////////////////////
// sorting uses ther function csg_compare
// also computes the sub/supergroups structure form
// the treesupergroup entry.
///////////////////////////////

  repeat 
  repeat 
    Sort(~grps, csg_compare2, ~perm2);
    permlist2 := ElementToSequence(perm2^-1);

    for g := 1 to #grps do
      grps[g]`supergroups := [permlist2[i] : i in grps[g]`supergroups]; 
      grps[g]`direct_supergroups := [permlist2[i] : i in grps[g]`direct_supergroups];
      grps[g]`subgroups := [permlist2[i] : i in grps[g]`subgroups];
      grps[g]`direct_subgroups := [permlist2[i] : i in grps[g]`direct_subgroups];
    end for; 
  print "sorted", perm2;
  until permlist2 eq [1..#grps];

    Sort(~grps, csg_compare, ~perm);
    permlist := ElementToSequence(perm^-1);

    for g := 1 to #grps do
      grps[g]`supergroups := [permlist[i] : i in grps[g]`supergroups]; 
      grps[g]`direct_supergroups := [permlist[i] : i in grps[g]`direct_supergroups];
      grps[g]`subgroups := [permlist[i] : i in grps[g]`subgroups];
      grps[g]`direct_subgroups := [permlist[i] : i in grps[g]`direct_subgroups];
    end for; 
  print "sorted", perm;
  until permlist eq [1..#grps];

end procedure;




csg_list_test_subgroups := function(grps)

 ret := [];

  for g := 1 to #grps do
    for s in grps[g]`direct_subgroups do
      if not g in grps[s]`direct_supergroups then
        Append(~ret,[g,s]);
      end if;
    end for;
  end for;
  return ret;
end function;





csg_list_label := procedure(~grps)
///////////////////////////////
// label the groups in grps
///////////////////////////////

//  Sort(~grps, csg_compare);

  genus := 0;
  level := 0;

  for i := 1 to #grps do
    if grps[i]`genus gt genus then
      level := grps[i]`level;
      genus := grps[i]`genus;
      name_count := 1;
    elif grps[i]`level gt level then
      level := grps[i]`level;
      name_count := 1;
    else 
      name_count +:= 1;
    end if;
    //////////////////////////////////
    // label the group
    //////////////////////////////////
    if name_count le 26 then
      grps[i]`label := CodeToString(64+name_count); 
    else
      a := name_count mod 26;
      b := (name_count-1) div 26;
      if a eq 0 then a := 26; end if;
        grps[i]`label := CodeToString(64+b) cat
                            CodeToString(64+a);
    end if; 
    grps[i]`name := IntegerToString(level) cat 
                          grps[i]`label cat
                          IntegerToString(genus);  
    print i, ":", grps[i]`name, "supergroups",grps[i]`direct_supergroups;
  end for; 
end procedure;


csg_list_complete_data := procedure(~grps)
//////////////////////////////////////////
// add extra fields
//////////////////////////////////////////

  for i := 1 to #grps  do
  g := grps[i];
//print "group ",g`name;
//print "order";
    g`order:=#sl2(g`level)/g`index;
//print "length";
    g`length:=csg_length(g);
//print "cusps";
    g`cusps:= csg_cusp_widths(g);
//print "c2";
    g`c2:= csg_c2(g);
//print "c3";
    g`c3:= csg_c3(g);
//print "mat";
    g`matgens:= Sort(csg_matrix_generators(g));
//print "galois";
    g`gc:= csg_galois_conjugates(g);
    grps[i] := g;
  end for;
end procedure;


csg_list_special_names := procedure(~grps)
//////////////////////////////////////////
// add special names. 
//////////////////////////////////////////

  //set special names
  for i:=1 to #grps do
    grps[i]`special_name:=[];
    grps[i]`special_name_html:=[];
  end for;

  for i:=1 to #grps do
    g := grps[i];
    if g`index eq 1 then
      Append(~g`special_name,"\\\overline\\\Gamma");
      Append(~g`special_name_html,"&Gamma;");
    else
      if (g`level eq 2) and (g`index eq 6) then
        Append(~g`special_name,"\\\overline\\\Gamma(2)");
        Append(~g`special_name_html,"&Gamma;(2)");
      end if;
      if g`index eq (mu(g`level)/2) and (g`level ne 2) then
        Append(~g`special_name,
             "\\\overline\\\Gamma("*IntegerToString(g`level)*")");
        Append(~g`special_name_html,
             "&Gamma;("*IntegerToString(g`level)*")");
      end if;
      SG:=sl2(g`level);
      GG,DI:=gl2(g`level);
      ph:= hom<sl2(g`level) -> GG | GG.1,GG.2>;
      g1:=sub<GG|DI,DI^ph(SG.1),ph(SG.2)>;
      g2:=ph(SG);
      g3:= g1 meet g2;
      g4:=ph(csg_perm_group(g));
      if IsConjugate(g2,g3,g4) then
        Append(~g`special_name,
            "\\\overline\\\Gamma_0("*IntegerToString(g`level)*")");
        Append(~g`special_name_html,
             "&Gamma;<sub>0</sub>("*IntegerToString(g`level)*")");
      end if;
      ga1:=sub<SG|SG.1^2,SG.2>;
      if IsConjugate(SG,ga1,csg_perm_group(g)) then
        Append(~g`special_name,
            "\\\overline\\\Gamma_1("*IntegerToString(g`level)*")");
        Append(~g`special_name_html,
            "&Gamma;<sub>1</sub>("*IntegerToString(g`level)*")");
      end if;
      if (g`level eq 2) and (g`index eq 2) then
        Append(~g`special_name,"\\\overline\\\Gamma^2");
        Append(~g`special_name_html,"&Gamma;<sup>2</sup>");
      end if;
      if (g`level eq 3) and (g`index eq 3) then
        Append(~g`special_name,"\\\overline\\\Gamma^3");
        Append(~g`special_name_html,"&Gamma;<sup>3</sup>");
      end if;
      if (g`index eq 6) and (g`level eq 6) and (g`length eq 1) then
        Append(~g`special_name,"\\\overline\\\Gamma'");
        Append(~g`special_name_html,"&Gamma;'");
      end if;
    end if;
    grps[i] := g;
  end for;

end procedure;




csg_list_fill := procedure(~L)

print "Computing further invariants";
  csg_list_complete_data(~L);
print "Special names";
  csg_list_special_names(~L);
print "Subgroups";
  csg_list_direct_subgroups(~L);
print "Sorting";
  csg_list_sort(~L);
print "Labeling";
  csg_list_label(~L);
end procedure;

/////////////////////////////////////////////////////////////////////



csg_list_write_by_genus := procedure(L)

  R := "ERROR";
 
  maxgen := 0;

  for grp in L do
    if grp`genus gt maxgen then
      maxgen := grp`genus;
    end if;
  end for;

for gen := 0 to maxgen do
  A := csg_list_by_genus(L,gen);
  S := "csg" cat IntegerToString(gen) cat ".det";
  SetLogFile(S);
  print "// list of all congruence groups with genus up to",maxgen;
  print "// part",gen,"of",maxgen;
  print "//";
  print "// by c cummins and s pauli" ;
  if gen gt 0 then
    print "load \"" cat R cat "\";";
    print "L cat:=";
  else
    print "L :=";
  end if;
  PrintFileMagma(S, A);
  print ";";
  UnsetLogFile();
  R := S;
end for;

end procedure;




csg_list_write_by_level := procedure(L, step)

  R := "ERROR";

  maxgen := 0;

  for grp in L do
    if grp`genus gt maxgen then
      maxgen := grp`genus;
    end if;
  end for;


  maxlev := 0;

  for grp in L do
    if grp`level gt maxlev then
      maxlev := grp`level;
    end if;
  end for;

  maxl := maxlev div step;

  print "MAXIMAL GENUS",maxgen;
  print "MAXIMAL LEVEL",maxlev;
 
  for gen := 0 to maxgen do
    for lev := 0 to maxl do
      B := csg_list_by_genus(L,gen);
      A := csg_list_by_levels(B,step*lev,step);
      if #A ne 0 then
        S := "csg" cat IntegerToString(gen) cat
           "-lev" cat IntegerToString(step*lev) cat ".dat";
        SetLogFile(S);
        print "// list of all congruence groups with genus ",gen;
        print "// with ",step*lev,"<= level <",step*lev+step;
        print "//";
        print "// by c cummins and s pauli" ;
        if lev gt 0 or gen gt 0 then
          print "load \"" cat R cat "\";";
          print "L cat:=";
        else
          print "L :=";
        end if;
        PrintFileMagma(S, A);
        print ";";
        UnsetLogFile();
        R := S;
      end if;
    end for;
  end for;

  // now a file with a nice name that loads the last
  S := "csg" cat IntegerToString(maxgen) cat ".dat";
  SetLogFile(S);
  print "load \"" cat R cat "\";";
  UnsetLogFile();

end procedure;










//////////////////////////////////////////////////////////////////////
// JUNK
//////////////////////////////////////////////////////////////////////

/* for computations by hand

  L := csg(16);
save "temp1.mw";
print "Computing further invariants";
  csg_list_complete_data(~L);
save "temp2.mw";
print "Special names";
  csg_list_special_names(~L);
save "temp3.mw";
print "Sorting";
  Sort(~L,csg_compare);
save "temp4.mw";
print "Labeling";
  L := csg_list_label(L);
save "temp5.mw";
print "Computing subgroup structure";
  csg_list_tree_to_subgroups(~L);
save "temp6.mw";            

*/







csg_list_galois_conj := procedure(~grps)

  for i := 1 to #grps  do
    g := grps[i];
    g`gc:= csg_galois_conjugates(g);
    grps[i] := g;
  end for;
end procedure;



csg_list_matgens := procedure(~grps)

  for i := 1 to #grps  do
    g := grps[i];
    g`matgens:= Sort(csg_matrix_generators(g));
    grps[i] := g;
  end for;
end procedure;


















/*
for g in L do
  delete g`supergroups;
  delete g`subgroups;
end for;
*/
