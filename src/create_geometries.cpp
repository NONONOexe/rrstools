#include "cpp11.hpp"
#include <unordered_map>

// Create LINESTRING sfg objects from start/end coordinate vectors.
// Each linestring is stored as a numeric vector with dim=c(2,2) and
// class=c("XY", "LINESTRING", "sfg"), matching sf's internal representation.
[[cpp11::register]]
cpp11::list create_linestrings_cpp(cpp11::doubles x1, cpp11::doubles y1,
                                   cpp11::doubles x2, cpp11::doubles y2) {
  R_xlen_t n = x1.size();
  cpp11::writable::list result(n);

  cpp11::writable::integers dim = {2, 2};
  cpp11::writable::strings  cls = {"XY", "LINESTRING", "sfg"};

  for (R_xlen_t i = 0; i < n; i++) {
    // R stores matrices in column-major order
    cpp11::writable::doubles mat(4);
    mat[0] = x1[i]; mat[1] = x2[i];  // column 0: X coords
    mat[2] = y1[i]; mat[3] = y2[i];  // column 1: Y coords
    mat.attr("dim")   = dim;
    mat.attr("class") = cls;
    result[i] = mat;
  }
  return result;
}

// Build edge ID → index hash map, then create MULTILINESTRING sfg objects
// directly, combining lookup and geometry construction in a single pass.
[[cpp11::register]]
cpp11::list create_face_multilinestrings_cpp(cpp11::list face_edge_hrefs,
                                             cpp11::strings edge_ids,
                                             cpp11::list edge_geoms) {
  std::unordered_map<std::string, int> index;
  index.reserve(edge_ids.size());
  for (R_xlen_t i = 0; i < edge_ids.size(); ++i) {
    index[std::string(edge_ids[i])] = static_cast<int>(i);
  }

  R_xlen_t n_faces = face_edge_hrefs.size();
  cpp11::writable::list result(n_faces);
  cpp11::writable::strings mls_cls = {"XY", "MULTILINESTRING", "sfg"};

  for (R_xlen_t f = 0; f < n_faces; f++) {
    cpp11::strings hrefs = face_edge_hrefs[f];
    R_xlen_t m = hrefs.size();
    cpp11::writable::list mls(m);
    for (R_xlen_t i = 0; i < m; i++) {
      auto it = index.find(std::string(hrefs[i]));
      if (it != index.end()) {
        mls[i] = edge_geoms[it->second];
      }
    }
    mls.attr("class") = mls_cls;
    result[f] = mls;
  }
  return result;
}
