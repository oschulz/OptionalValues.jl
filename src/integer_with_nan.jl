# This file is a part of OptionalValues.jl, licensed under the MIT License (MIT).


struct IntegerWithNaN{T<:Integer,E<:Integer} <: Integer
    encoded::E
end


Base.isnan(x::IntegerWithNaN{T,E}) where {T,N} =
    x.encoded == encode_nan(E)


Base.promote_rule(::Type{T}, ::Type{IntegerWithNaN{U}}) where {T<:Integer,U} =
    with_nan(promote_type(T,U))

Base.promote_rule(::Type{T}, ::Type{IntegerWithNaN{U}}) where {T<:AbstractFloat,U} =
    float(promote_type(T,U))


function Base.convert(TN::Type{IntegerWithNaN{T,E}}, x::U) where {T,E,U<:Integer}
    enc_x = convert(E, x)
    if typemax(E) > typemax(U)
        TN(enc_x)
    else
        if enc_x != encode_nan(E)
            TN(enc_x)
        else
            throw(InexactError())
        fi
    end
end

function Base.convert(T::Type{<:Integer}, x::IntegerWithNaN)
    if !isnan(x)
        convert(T, x.encoded)
    else
        throw(InexactError())
    end
end

function Base.convert(TN::Type{<:IntegerWithNaN{T,E}}, x::IntegerWithNaN) where {T,E}
    if !isnan(x)
        TN(convert(E, convert(T, x.encoded)))
    else
        intnan(T)
    end
end


Base.rem(x::IntegerWithNaN, ::Type{<:Integer}) =
    rem(x.encoded, T)

Base.rem(x::Integer, TN::Type{IntegerWithNaN{T,E}}) where {T,E} =
    TN(rem(rem(x, T), E))

Base.rem(x::IntegerWithNaN, TN::Type{<:IntegerWithNaN{T,E}}) =
    TN(rem(x.encoded, E))




function with_nan end
export with_nan

@static with_nan(::Type{Int8}) = IntegerWithNaN{Int8,Int32}
@static with_nan(::Type{UInt8}) = IntegerWithNaN{UInt8,UInt32}
@static with_nan(::Type{Int16}) = IntegerWithNaN{Int16,Int32}
@static with_nan(::Type{UInt16}) = IntegerWithNaN{UInt16,UInt32}
@static with_nan(::Type{Int32}) = IntegerWithNaN{Int32,Int32}
@static with_nan(::Type{UInt32}) = IntegerWithNaN{UInt32,UInt32}
@static with_nan(::Type{Int64}) = IntegerWithNaN{Int64,Int64}
@static with_nan(::Type{UInt64}) = IntegerWithNaN{UInt64,UInt64}

with_nan(x::Integer) = convert(with_nan(typeof(x)), x)


function intnan end
export intnan

@static intnan(TN::Type{IntegerWithNaN{T,E}}) where {T,E} = TN(encode_nan(E))

intnan(T::Type{<:Integer}) = intnan(with_nan(T))


@static encode_nan(E::Type{<:Integer}) = typemax(E)





