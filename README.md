# Flatbuffers Builder

Haskell library for flatbuffers serialization. Users of this library
must first build an `Object`, a flatbuffers syntax tree, from their
data. After that, `encode` serializes the object. This is not a
high performance solution, but it works pretty well.

Currently, this library doesn't do anything particularly intelligent
to keep the output small. Everything ends up 8-byte aligned, which is
not necessary in a lot of cases. This alignment causes there to be
a lot of padding. Also, virtual tables don't get shared like they
do by many other flatbuffers serializers. These things can be improved
without changing the user-facing interface. There just isn't a need
to do so at the moment.
