class Program:
    def __init__(self, classes):
        self.classes = classes


class ClassDef:
    def __init__(self, type, features):
        self.type = type
        self.features = features


class Feature:
    def __init__(self, name, formals, type):
        self.name = name
        self.formals = formals
        self.type = type
