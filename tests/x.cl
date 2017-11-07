class AList {

   head() : Int { { abort(2); 0; } };
    tail(): Int { { abort(2); 0; } };
};

class BList {
    tail(): Int { { abort(); 0; } };

};