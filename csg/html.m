
load "table.m";

///////////////////////////////////////////////////////////////////////

part_to_html_string:=function(part);
  p:=part;
  c:=" ";
  cv:=p[1];
  ex:=0;
  for i:=1 to #p do
    if p[i] ne cv then
      c:=c cat IntegerToString(cv) cat "<sup>"*IntegerToString(ex) cat "</sup>";
      cv:=p[i];
      ex:=1;
    else
      ex:=ex+1;
    end if;
  end for;
  c:=c cat IntegerToString(cv) cat "<sup>" cat IntegerToString(ex) cat "</sup>";
  return c;
end function;

html_part := procedure(part)
  p:=part;
  cv:=p[1];
  ex:=0;
  for i:=1 to #p do
    if p[i] ne cv then
      print IntegerToString(cv) cat "<sup>"*IntegerToString(ex) cat "</sup>";
      cv:=p[i];
      ex:=1;
    else
      ex:=ex+1;
    end if;
  end for;
  print IntegerToString(cv) cat "<sup>" cat IntegerToString(ex) cat "</sup>";
end procedure;
 




html_header_common := procedure(level)

    levelname := "\"level" cat IntegerToString(level) cat "\"";
    print "<tr id=",levelname,">";
    print "<th class =\"white\"><a href=\"#top\">^</a></th>";
    print "<th class =\"common\">Level&nbsp;"
            cat IntegerToString(level) cat "</th>";
    print "<td class =\"common\">Name</td>";
    print "<td class =\"common\">Index</td>";
    print "<td class =\"common\">&nbsp;con&nbsp;</td>";
    print "<td class =\"common\">&nbsp;len&nbsp;</td>";
    print "<td class =\"common\">&nbsp;c<sub>2</sub>&nbsp;</td>";
    print "<td class =\"common\">&nbsp;c<sub>3</sub>&nbsp;</td>";
end procedure;



html_header_blue := procedure(level,genus)

    html_header_common(level);
    greenlink := "\"csg" cat IntegerToString(genus)
                         cat "M.html#"
                         cat "level"
                         cat IntegerToString(level)
                         cat "\""; 

    print "<th class =\"green\"><a href=" cat greenlink cat ">&gt;</a></th>";
    print "<td class =\"blue\">Cusps</td>";
    print "<td class =\"blue\">Gal</td>";
    print "<td class =\"blue\">&nbsp;Supergroups&nbsp;</td>";
    print "<td class =\"blue\">&nbsp;Subgroups&nbsp;</td>";
    print "</tr>";    
 
end procedure;



html_header_green := procedure(level,genus)

    html_header_common(level);
    bluelink := "\"csg" cat IntegerToString(genus)
                         cat ".html#"
                         cat "level"
                         cat IntegerToString(level)
                         cat "\""; 
    print "<th class =\"blue\"><a href=" cat bluelink cat ">&gt;</a></th>";
    print "<td class =\"green\">";
    print "SL(2,Z/" cat IntegerToString(level) cat "Z) Matrix Generators</td>";
    print "</tr>";    
 
end procedure;


html_label := function(grp)

  return IntegerToString(grp`level)*grp`label*"<sup>"*
         IntegerToString(grp`genus)*"</sup>";

end function;


html_cols_common := procedure(grp)

          grpname := "\"group" cat grp`name cat "\"";
          print "<tr id=",grpname,">";
          // up
          print "<td>&nbsp;</td>";
          // Label -- !! use superscript
          print "<th align=center>", html_label(grp), "</th>";
          // Name 
          print "<td align=center>"; 
          if assigned grp`special_name_html then
            for i := 1 to #grp`special_name_html do
              if i ne #grp`special_name_html then
                print grp`special_name_html[i] cat ",";
              else
                print grp`special_name_html[i];
              end if;
            end for;
          end if;
          print "</td>";
          // Index
          print "<td align=right>", grp`index, "</td>";
          // #Aut
          print "<td align=right>";
          if assigned grp`GLZConj then
            print IntegerToString(grp`GLZConj);
          end if;
          print "</td>";
          // #Con -- length
          print "<td align=right>";
          if assigned grp`length then
            print IntegerToString(grp`length);
          end if;
          print "</td>";
          // c2
          print "<td align=right>";
          if assigned grp`c2 then
            print IntegerToString(number_of_fixed_points(grp`c2));
          end if;
          print "</td>";
          // c3
          print "<td align=right>";
          if assigned grp`c3 then
            print IntegerToString(number_of_fixed_points(grp`c3));
          end if;
          print "</td>";
          // switch
          print "<td>";
          print "</td>";

end procedure;


html_head_common := procedure(genus, genL, maxgenus, maxlevel)

    print "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"> ";
    print "<HTML><HEAD><TITLE>";
    print "Congruence Subgroups of Genus",genus;
    print "</TITLE >";
    print "<link rel=stylesheet type=\"text/css\" href=\"csg.css\">";
    print "</HEAD>";
    print "<BODY>";
    print "<p><strong>";
    print "<a name=\"top\" href =\"congruence.html\">[Introduction]</a> - ";
    print "Groups of Genus: ";
    print "</strong>";
    for gen := 0 to maxgenus do
      genusfilename := "\"csg" cat IntegerToString(gen) cat ".html\"";
      print "<a href =",genusfilename,">" cat IntegerToString(gen)
                                          cat "</a>";
    end for;
    print "<H1> Congruence Subgroups of PSL(2,Z) of Genus",genus,"</H1>";
    print "<p><strong>";
    print "Groups of Level: ";
    print "</strong>";
    for level := 1 to maxlevel do
      levL := csg_list_by_level(genL,level);
      if #levL ne 0 then
        levelname := "\"#level" cat IntegerToString(level) cat "\"";
        print "<a href =",levelname,">" cat IntegerToString(level)
                                         cat"</a>";
      end if;
    end for;
    print "<br>&nbsp;<br>&nbsp;<br>";

end procedure;


html_address := procedure()

    print "<address>";
    print "<a href=\"http://cicma.mathstat.concordia.ca/faculty/cummins/welcome.html\">";    
    print "Chris Cummins</a> and ";
    print "<a href=\"http://www.math.tu-berlin.de/~pauli\">";
    print "Sebastian Pauli</a>, computed with";
    print "<a href =\"http://magma.maths.usyd.edu.au/magma/\">";
    print "MAGMA</a>";
    print "</address>";
    print "</BODY></HTML>";

end procedure;

//////////////////////////////////////////////////////////////////////////

csg_list_to_html_blue := procedure(grpsin)

  grps := grpsin;

  maxgenus := 0;
  maxindex := 1;
  maxlevel := 1;

  for grp in grps do
    if grp`level gt maxlevel then
      maxlevel := grp`level;
    end if;
    if grp`genus gt maxgenus then
      maxgenus := grp`genus;
    end if;
    if grp`index gt maxindex then
      maxindex := grp`index;
    end if;
  end for;


  for genus := 0 to maxgenus do
    genL := csg_list_by_genus(grps,genus);
    filename := "csg" cat IntegerToString(genus) cat ".html";
    SetLogFile(filename);
    html_head_common(genus, genL, maxgenus, maxlevel);
    print "<table>";
//    print "<caption>&nbsp;</caption>";
    for level := 1 to maxlevel do
      levL := csg_list_by_level(genL,level);
      if #levL ne 0 then
        html_header_blue(level,genus);
        for grp in levL do

          html_cols_common(grp);

          // Cusps
          print "<td align=right>";
          if assigned grp`cusps then
            html_part(grp`cusps);
          end if;
          print "</td>";
          // Gal
          print "<td align=right>";
          if assigned grp`gc then
            html_part(grp`gc);
          end if;
          print "</td>";
          // Supergroups
          print "<td align=left>";
          for sup in grp`direct_supergroups do
            link := "\"csg" cat IntegerToString(grps[sup]`genus)
                          cat ".html#"
                          cat "group"
                          cat grps[sup]`name
                          cat "\"";
            print "<a href =",link,">" cat html_label(grps[sup]) cat "</a>";
          end for;
          print "</td>";
          // Subgroups
          print "<td align=left>";
          for sub in grp`direct_subgroups do
            link := "\"csg" cat IntegerToString(grps[sub]`genus)
                          cat ".html#"
                          cat "group"
                          cat grps[sub]`name
                          cat "\"";
            print "<a href =",link,">" cat html_label(grps[sub]) cat "</a>";
          end for;
          print "</td>";
          print "</tr>";
        end for;
      end if;
    end for;
    print "</table>";
//    print "<br>&nbsp;<br>";
    html_address();
    UnsetLogFile();
  end for;
end procedure;



csg_list_to_html_green := procedure(grpsin)

  grps := grpsin;

  maxgenus := 0;
  maxindex := 1;
  maxlevel := 1;

  for grp in grps do
    if grp`level gt maxlevel then
      maxlevel := grp`level;
    end if;
    if grp`genus gt maxgenus then
      maxgenus := grp`genus;
    end if;
    if grp`index gt maxindex then
      maxindex := grp`index;
    end if;
  end for;


  for genus := 0 to maxgenus do
    genL := csg_list_by_genus(grps,genus);
    filename := "csg" cat IntegerToString(genus) cat "M.html";
    SetLogFile(filename);
    html_head_common(genus, genL, maxgenus, maxlevel);
    print "<table>";
//    print "<caption>&nbsp;</caption>";
    for level := 1 to maxlevel do
      levL := csg_list_by_level(genL,level);
      if #levL ne 0 then
        html_header_green(level,genus);
        for grp in levL do

          html_cols_common(grp);
          
          // matrix generators
          if assigned grp`matgens then
            print "<td align=left>";
            for i := 1 to #grp`matgens do
              if i ne #grp`matgens then
                print grp`matgens[i],",";
              else
                print grp`matgens[i];
              end if;
            end for;
            print "</td>";
          end if;
          print "</tr>";
        end for;
      end if;
    end for;
    print "</table>";
//    print "<br>&nbsp;<br>";
    html_address();
    UnsetLogFile();
  end for;
end procedure;

////////////////////////////////////////////////////////////////////////

csg_list_to_html_stat := procedure(grps)
 
  maxgenus := 0;
  maxindex := 1;
  maxlevel := 1;

  for grp in grps do
    if grp`level gt maxlevel then
      maxlevel := grp`level;
    end if; if grp`genus gt maxgenus then
      maxgenus := grp`genus;
    end if;
    if grp`index gt maxindex then
      maxindex := grp`index;
    end if;
  end for;

  SetLogFile("csg-stat.html");

    print "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"> ";
    print "<HTML><HEAD><TITLE>";
    print "Congruence Subgroups: Statistic";
    print "</TITLE >";
    print "<link rel=stylesheet type=\"text/css\" href=\"csg.css\">";
    print "</HEAD>";
    print "<BODY>";
    print "<p><strong>";
    print "<a name=\"top\" href =\"congruence.html\">[Introduction]</a> - ";
    print "Groups of Genus: ";
    print "</strong>";
    for gen := 0 to maxgenus do
      genusfilename := "\"csg" cat IntegerToString(gen) cat ".html\"";
      print "<a href =",genusfilename,">" cat IntegerToString(gen)
                                          cat "</a>";
    end for;
    print "<H1> Congruence Subgroups of PSL(2,Z)</H1>";

     amaxlev := 0;
     amaxind := 0;
     aall := 0;
     apsl := 0;
     apgl := 0;
     atfmaxlev := 0;
     atfmaxind := 0;
     atfall := 0;
     atfpsl := 0;
     atfpgl := 0;
     acmaxlev := 0;
     acmaxind := 0;
     acall := 0;
     acpsl := 0;
     acpgl := 0;

  print "<p><strong>";
  print "<a href=\"congruence.html#tables\">[Tables]</a> -";
  print "<a href=\"csg-stat.html\">[Statistics]</a> -";
  print "MAGMA: <a href=\"congruence.html#programs\">[Functions]</a>";
  print "<a href=\"congruence.html#data\">[Data]</a> -";
  print "Mirrors:";
  print "<a href=\"http://www.math.tu-berlin.de/~pauli/congruence\" ";
  print ">[Berlin]</a>";
  print "<a href=\"http://www.mathstat.concordia.ca/faculty/cummins/congruence\" ";
  print ">[Montreal]</a>";


print "</strong><br>&nbsp;<br>";
 
  print "<H3><a href=\"congruence.html#top\">^</a> Statistics</H3>";
  print "<p>";
  print "The table below gives the number of congruence subgroups of genus";
  print "0 to " cat IntegerToString(maxgenus) cat ",";
  print "giving the total number, the number of congruence subgroups up";
  print " to conjugacy in PSL(2,Z) and PGL(2,Z),";
  print "and the maximal index and maximal level for each genus.";
  print "The same is data is also given for the torsion-free subgroups";
  print "(the groups with c<sub>2</sub>=c<sub>3</sub>=0 in the main tables)";
  print "and the cycloidal subgroups (the groups with one equivalence class ";
  print "of cusps).";
  print "<br>&nbsp;<br>";
  print "<table class=\"data\" cellspacing=2 cellpadding=3>";
  print "<tr>";
  print "<td></td>";
  print "<td class=\"blue\" colspan=\"5\">All Groups</td>";
  print "<td class=\"green\" colspan=\"5\">Torsion Free Groups</td>";
  print "<td class=\"blue\" colspan=\"5\">Cycloidal Groups</td>";
  print "</tr>";
  print "<tr>";
  print "<th class=\"common\">Genus</th>";
  print "<td class=\"blue\">#All</td><td class=\"blue\">#PSL</td><td class=\"blue\">#PGL</td><td class=\"blue\">Max. Level</td><td class=\"blue\">Max. Index</td>";
  print "<td class=\"green\">#All</td><td class=\"green\">#PSL</td><td class=\"green\">#PGL</td><td class=\"green\">Max. Level</td><td class=\"green\">Max. Index</td>";
  print "<td class=\"blue\">#All</td><td class=\"blue\">#PSL</td><td class=\"blue\">#PGL</td><td class=\"blue\">Max. Level</td><td class=\"blue\">Max. Index</td>";
 
  for genus := 0 to maxgenus do
     print "<tr>";
     genusfilename := "\"csg" cat IntegerToString(genus) cat ".html\"";
     print "<th class=\"common\">" cat 
           "<a href =",genusfilename,">" cat
           IntegerToString(genus) cat "</a></th>";
     genL := csg_list_by_genus(grps,genus);
     maxlev := 0;
     maxind := 0;
     all := 0;
     psl := 0;
     pgl := 0;
     tfmaxlev := 0;
     tfmaxind := 0;
     tfall := 0;
     tfpsl := 0;
     tfpgl := 0;
     cmaxlev := 0;
     cmaxind := 0;
     call := 0;
     cpsl := 0;
     cpgl := 0;
     for grp in genL do

       c2 := number_of_fixed_points(grp`c2);
       c3 := number_of_fixed_points(grp`c3);
       pgl +:= 1;
       psl +:= grp`GLZConj;
       if grp`level gt maxlev then maxlev :=  grp`level; end if; 
       if grp`index gt maxind then maxind :=  grp`index; end if; 
       all +:= grp`GLZConj * grp`length;
       // is torsion free
       if c2 eq 0 and c3 eq 0 then
         tfpsl +:= grp`GLZConj;
         if grp`level gt tfmaxlev then tfmaxlev :=  grp`level; end if; 
         if grp`index gt tfmaxind then tfmaxind :=  grp`index; end if; 
         tfall +:= grp`GLZConj * grp`length;
         tfpgl +:= 1;
       end if;
       // is cycloidal
       if #grp`cusps eq 1 then
         cpsl +:= grp`GLZConj;
         if grp`level gt cmaxlev then cmaxlev :=  grp`level; end if; 
         if grp`index gt cmaxind then cmaxind :=  grp`index; end if; 
         call +:= grp`GLZConj * grp`length;
         cpgl +:= 1;
       end if;

     end for;


     amaxlev := Maximum(amaxlev,maxlev); 
     amaxind := Maximum(amaxind,maxind);
     aall +:= all;
     apsl +:= psl;
     apgl +:= pgl;
     atfmaxlev := Maximum(atfmaxlev,tfmaxlev) ;
     atfmaxind := Maximum(atfmaxind,tfmaxind) ;
     atfall +:= tfall;
     atfpsl +:= tfpsl;
     atfpgl +:= tfpgl;
     acmaxlev := Maximum(acmaxlev,cmaxlev);
     acmaxind := Maximum(acmaxind,cmaxind);
     acall +:= call;
     acpsl +:= cpsl;
     acpgl +:= cpgl;
 
       print "<td>" cat IntegerToString(all) cat "</td>";
       print "<td>" cat IntegerToString(psl) cat "</td>";
       print "<td>" cat IntegerToString(pgl) cat "</td>";
       print "<td>" cat IntegerToString(maxlev) cat "</td>";
       print "<td>" cat IntegerToString(maxind) cat "</td>";

       print "<td>" cat IntegerToString(tfall) cat "</td>";
       print "<td>" cat IntegerToString(tfpsl) cat "</td>";
       print "<td>" cat IntegerToString(tfpgl) cat "</td>";
       print "<td>" cat IntegerToString(tfmaxlev) cat "</td>";
       print "<td>" cat IntegerToString(tfmaxind) cat "</td>";

       print "<td>" cat IntegerToString(call) cat "</td>";
       print "<td>" cat IntegerToString(cpsl) cat "</td>";
       print "<td>" cat IntegerToString(cpgl) cat "</td>";
       print "<td>" cat IntegerToString(cmaxlev) cat "</td>";
       print "<td>" cat IntegerToString(cmaxind) cat "</td>";

       print "</tr>";

   end for;
        
       print "<tr>";
       print "<th class=\"common\"> All </th>";
 
       print "<td class=\"br\">" cat IntegerToString(aall) cat "</td>";
       print "<td class=\"br\">" cat IntegerToString(apsl) cat "</td>";
       print "<td class=\"br\">" cat IntegerToString(apgl) cat "</td>";
       print "<td class=\"br\">" cat IntegerToString(amaxlev) cat "</td>";
       print "<td class=\"br\">" cat IntegerToString(amaxind) cat "</td>";

       print "<td class=\"gr\">" cat IntegerToString(atfall) cat "</td>";
       print "<td  class=\"gr\">" cat IntegerToString(atfpsl) cat "</td>";
       print "<td class=\"gr\">" cat IntegerToString(atfpgl) cat "</td>";
       print "<td class=\"gr\">" cat IntegerToString(atfmaxlev) cat "</td>";
       print "<td class=\"gr\">" cat IntegerToString(atfmaxind) cat "</td>";

       print "<td class=\"br\">" cat IntegerToString(acall) cat "</td>";
       print "<td class=\"br\">" cat IntegerToString(acpsl) cat "</td>";
       print "<td class=\"br\">" cat IntegerToString(acpgl) cat "</td>";
       print "<td class=\"br\">" cat IntegerToString(acmaxlev) cat "</td>";
       print "<td class=\"br\">" cat IntegerToString(acmaxind) cat "</td>";

       print "</tr>";

   print "</table>";
   print "<p>"; 
   html_address();
   UnsetLogFile();

end procedure;
//////////////////////////////////////////////////////////////////////////

csg_list_to_html := procedure(grps)

  csg_list_to_html_blue(grps);
  csg_list_to_html_green(grps);
  csg_list_to_html_stat(grps);

end procedure;


