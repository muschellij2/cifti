parse_named_map = function(nodeset) {
  n_nodes = length(nodeset)
  all_attrs = xml_attrs(nodeset)

  make_num = function(x) {
    as.numeric(as.character(x))
  }
  mn = xml_find_all(nodeset, "./MapName")
  mn = xml_text(mn)

  lt = xml_find_all(nodeset, "./LabelTable")
  lt = lapply(lt, xml_find_all,
              xpath =  "./Label")
  # txt_lt = lapply(lt, xml_text)
  lt = lapply(lt, function(x) {
    att = xml_attrs(x)
    val = xml_text(x)
    att = mapply(function(x, y){
      c(x, "Label" = y)
    }, att, val, SIMPLIFY = FALSE)
    att = do.call("rbind", att)
    att = data.frame(att,
                     stringsAsFactors = FALSE)

    att$Key = make_num(att$Key)
    att$Red = make_num(att$Red)
    att$Green = make_num(att$Green)
    att$Blue = make_num(att$Blue)
    att$Alpha = make_num(att$Alpha)
    return(att)
  })
  if (length(lt) > 0) {
    names(lt) = mn
  }
  return(lt)
}
