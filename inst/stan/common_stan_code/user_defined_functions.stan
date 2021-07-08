functions {

    int get_number_indexes_same_value(int[] M, int M_i) {
        // given the missingness patterns matrix M and a missingness pattern M_i,
        // the number of patients with missingness pattern M_i is returned.
        int count = 0;
        for (n in 1:num_elements(M)) {
            if(M[n]==M_i) count = count+1;
        }
        return(count);
    }

    int[] get_indexes_same_value(int[] M, int dim, int M_i) {
        // given the missingness patterns matrix M and a missingness pattern M_i,
        // the patients rows with same missingness pattern M_i is returned.
        int return_vec[dim];
        int i = 1;
        for (n in 1:num_elements(M)) {
            if(M[n]==M_i) {
                return_vec[i] = n;
                i = i+1;
            }
        }
        return(return_vec);
    }

    matrix get_submatrix_bylogical(matrix mat, int[] logical) {
        // example: mat = [2,4,6 ; 1,3,5; 7,8,9], logical = [0,1,1] then
        // output = [3,5 ; 8,9]. Needed to access to subportion of covariance matrix

        int dim = sum(logical);
        matrix[dim,dim] submat;
        int index[dim] = get_indexes_same_value(logical, dim, 1);

        submat = mat[index, index];
        return submat;
    }

    vector[] to_vector_of_arrays(vector vec, int length_array) {
        // treansform a vector into a vector of arrays. Example: vec = [1,2,3,4,5,6] and
        // length_array = 2, then output = [1,2; 3,4; 5,6]
        vector[length_array] res[num_elements(vec)/length_array];

        int j = 1;
        int i = 1;
        while(j <= num_elements(vec)) {
            res[i,] = vec[j:(j+length_array-1)];
            i = i+1;
            j = j + length_array;
        }
        return(res);
    }

}
