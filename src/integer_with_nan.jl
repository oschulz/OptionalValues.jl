# This file is a part of OptionalValues.jl, licensed under the MIT License (MIT).


struct IntegerWithNaN{T<:Integer} <: Integer
    encoded::T

    IntegerWithNaN{T}() where {T<:Integer} = new(encode_nan(T))

    IntegerWithNaN{T}(x) where {T<:Integer} = new(x)
end

export IntegerWithNaN


function Base.show(io::IO, ::MIME"text/plain", x::IntegerWithNaN{T}) where T
    if !isnan(x)
        print(io, "withnan($(x.encoded))")
    else
        print(io, "$(T)NaN")
    end
end


Base.isnan(x::IntegerWithNaN{T}) where {T} =
    x.encoded == encode_nan(T)


Base.promote_rule(::Type{T}, ::Type{IntegerWithNaN{U}}) where {T<:Integer,U} =
    withnan(promote_type(T,U))

Base.promote_rule(::Type{Bool}, TN::Type{IntegerWithNaN{T}}) where {T} = TN

Base.promote_rule(::Type{T}, ::Type{IntegerWithNaN{U}}) where {T<:AbstractFloat,U} =
    float(promote_type(T,U))


Base.convert(TN::Type{IntegerWithNaN{T}}, x::Integer) where {T} = TN(x)

function Base.convert(T::Type{<:Integer}, x::IntegerWithNaN)
    if !isnan(x)
        convert(T, x.encoded)
    else
        throw(InexactError())
    end
end

function Base.convert(TN::Type{IntegerWithNaN{T}}, x::IntegerWithNaN) where {T}
    if !isnan(x)
        TN(convert(T, x.encoded))
    else
        TN()
    end
end

Base.convert(TN::Type{IntegerWithNaN{T}}, x::AbstractFloat) where {T} =
    ifelse(isnan(x), TN(), TN(convert(T, x)))

Base.convert(T::Type{<:AbstractFloat}, x::IntegerWithNaN) =
    ifelse(isnan(x), convert(T, NaN), convert(T, x.encoded))


Base.rem(x::IntegerWithNaN{T}, ::Type{<:Integer}) where {T} =
    rem(x.encoded, T)

Base.rem(x::Integer, TN::Type{IntegerWithNaN{T}}) where {T} =
    TN(rem(x, T))

Base.rem(x::IntegerWithNaN, TN::Type{IntegerWithNaN{T}}) where {T} =
    ifelse(isnan(x), TN(), TN(rem(x.encoded, T)))


import Base: ==, !=

for op in (:(==), :(!=))
    @eval begin
        $op(a::IntegerWithNaN, b::Number) =
            ifelse(isnan(a), false, $op(a.encoded, b))

        $op(a::Number, b::IntegerWithNaN) =
            ifelse(isnan(b), false, $op(a, b.encoded))

        $op(a::IntegerWithNaN, b::IntegerWithNaN) =
            ifelse(isnan(a) || isnan(b), false, $op(a.encoded, b.encoded))
    end
end


import Base: <, <=

for op in (:(<), :(<=))
    @eval begin
        $op(a::IntegerWithNaN, b::Real) =
            ifelse(isnan(a), false, $op(a.encoded, b))

        $op(a::Real, b::IntegerWithNaN) =
            ifelse(isnan(b), false, $op(a, b.encoded))

        $op(a::IntegerWithNaN, b::IntegerWithNaN) =
            ifelse(isnan(a) || isnan(b), false, $op(a.encoded, b.encoded))
    end
end


import Base: +, -, *, /, ^

for op in (:(+), :(-), :(*), :(/), :(^))
    @eval begin
        function $op(a::IntegerWithNaN, b::Number)
            R = _result_type(a, b)
            ifelse(isnan(a), nanvalue(R), convert(R, $op(a.encoded, b)))
        end

        function $op(a::IntegerWithNaN, b::Bool)
            R = _result_type(a, b)
            ifelse(isnan(a), nanvalue(R), convert(R, $op(a.encoded, b)))
        end

        function $op(a::Number, b::IntegerWithNaN)
            R = _result_type(a, b)
            ifelse(isnan(b), nanvalue(R), convert(R, $op(a, b.encoded)))
        end

        function $op(a::Bool, b::IntegerWithNaN)
            R = _result_type(a, b)
            ifelse(isnan(b), nanvalue(R), convert(R, $op(a, b.encoded)))
        end

        function $op(a::IntegerWithNaN, b::IntegerWithNaN)
            R = _result_type(a, b)
            ifelse(isnan(a) || isnan(b), nanvalue(R), convert(R, $op(a.encoded, b.encoded)))
        end
    end
end


import Base: div, fld, cld, rem, mod, mod1, fld1, max, min, minmax

for op in (:(div), :(fld), :(cld), :(rem), :(mod), :(mod1), :(fld1), :(max), :(min), :(minmax))
    @eval begin
        function $op(a::IntegerWithNaN, b::Real)
            R = _result_type(a, b)
            ifelse(isnan(a), nanvalue(R), convert(R, $op(a.encoded, b)))
        end

        function $op(a::Real, b::IntegerWithNaN)
            R = _result_type(a, b)
            ifelse(isnan(b), nanvalue(R), convert(R, $op(a, b.encoded)))
        end

        function $op(a::IntegerWithNaN, b::IntegerWithNaN)
            R = _result_type(a, b)
            ifelse(isnan(a) || isnan(b), nanvalue(R), convert(R, $op(a.encoded, b.encoded)))
        end
    end
end



#=

invmod
gcdx

gcd(a::Integer) = a
lcm(a::Integer) = a

powermod

nextpow2(x::Integer)
prevpow2(x::Integer)
ispow2(x::Integer)
isqrt(n::Integer)

factorial(n::Integer)
binomial(n::T, k::T) where T<:Integer


nextpow(a::Real, x::Real)

prevpow(a::Real, x::Real)

floor
ceil
<<
>>


ndigits0z
ndigits0z
ndigitsnb
ndigits(x::Unsigned, b::Integer)
ndigits(x::Unsigned)
bin(x::Unsigned, pad::Int, neg::Bool)
oct(x::Unsigned, pad::Int, neg::Bool)
dec(x::Unsigned, pad::Int, neg::Bool)
hex(x::Unsigned, pad::Int, neg::Bool)

base(b::Int, x::Unsigned, pad::Int, neg::Bool)
bin(n, pad::Int=1)
hex(n, pad::Int=1)
oct(n, pad::Int=1)
dec(n, pad::Int=1)

digits([T<:Integer], n::Integer, base::T=10, pad::Integer=1)

=#




function withnan end
export withnan

Base.@pure withnan(::Type{Int8}) = IntegerWithNaN{Int16}
Base.@pure withnan(::Type{UInt8}) = IntegerWithNaN{UInt16}
Base.@pure withnan(::Type{Int16}) = IntegerWithNaN{Int32}
Base.@pure withnan(::Type{UInt16}) = IntegerWithNaN{UInt32}
Base.@pure withnan(::Type{Int32}) = IntegerWithNaN{Int32}
Base.@pure withnan(::Type{UInt32}) = IntegerWithNaN{UInt32}
Base.@pure withnan(::Type{Int64}) = IntegerWithNaN{Int64}
Base.@pure withnan(::Type{UInt64}) = IntegerWithNaN{UInt64}

# Base.@pure withnan(::Type{Bool}) = IntegerWithNaN{Int8}

withnan(x::Integer) = convert(withnan(typeof(x)), x)


function nanvalue end
export nanvalue

Base.@pure nanvalue(TN::Type{IntegerWithNaN{T}}) where {T} = TN()
Base.@pure nanvalue(T::Type{<:AbstractFloat}) = T(NaN)

Base.@pure encode_nan(T::Type{<:Integer}) = typemax(T)


_result_type(a::T, b::U) where {T,U} = promote_type(T, U)


export IntNaN, Int32NaN, Int64NaN, UInt32NaN, UInt64NaN

const IntNaN = IntegerWithNaN{Int}()
const Int32NaN = IntegerWithNaN{Int32}()
const Int64NaN = IntegerWithNaN{Int64}()
const UIntNaN = IntegerWithNaN{UInt}()
const UInt32NaN = IntegerWithNaN{Int32}()
const UInt64NaN = IntegerWithNaN{Int64}()
