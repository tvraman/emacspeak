
%$Id$
% package 3d
input 3d/3d
% polyhedra definitions
input 3d/poly

% set scale
drawing_scale:=8cm;
filled_faces:=true; % default value


% labels is a list of labels one per image
vardef animate_three_objects_and_label(expr name_a,name_b,name_c,imin,imax,index,labels)=
  numeric ang;ang=360/(imax-imin+1);
  assign_obj("obja",name_a);assign_obj("objb",name_b);
  assign_obj("objc",name_c);
  assign_obj("labelList", labels);
  scale_obj("objb",.7);
  numeric v_a;v_a:=new_vect;
  vect_def_vect(v_a,vect_K);vect_mult(v_a,v_a,4);put_obj("objb",v_a,1,0,0,0);
  free_vect(v_a);
  scale_obj("objc",.5);
  translate_obj("objc",vect_K);translate_obj("objc",vect_K);
  labeloffset:=10pt;
defaultfont:="ptmb8r";
  for i:=imin upto imax:
    one_image_three_objects("obja","objb","objc",i+index,i,7,ang);
dotlabel.ulft ("Emacspeak",(0,0));
  endfor;
  show_animation_bbox;
enddef;


animate_three_objects_and_label ("octahedron",%
  "dodecahedron",%
  "icosahedron",
  1, 100, 100
  );
end
