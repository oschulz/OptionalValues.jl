# This file is a part of OptionalValues.jl, licensed under the MIT License (MIT).


struct IntegerWithNaN{T<:Integer,E<:Integer} <: Integer
    encoded::E
end


Base.isnan(x::IntegerWithNaN{T,E}) where {T,E} =
    x.encoded == encode_nan(E)


Base.promote_rule(::Type{T}, ::Type{IntegerWithNaN{U}}) where {T<:Integer,U} =
    withnan(promote_type(T,U))

Base.promote_rule(::Type{T}, ::Type{IntegerWithNaN{U}}) where {T<:AbstractFloat,U} =
    float(promote_type(T,U))


function Base.convert(TN::Type{IntegerWithNaN{T,E}}, x::Integer) where {T,E}
    enc_x = convert(E, x)
    if typemax(E) > typemax(x)
        TN(enc_x)
    else
        if enc_x != encode_nan(E)
            TN(enc_x)
        else
            throw(InexactError())
        end
    end
end

function Base.convert(T::Type{<:Integer}, x::IntegerWithNaN)
    if !isnan(x)
        convert(T, x.encoded)
    else
        throw(InexactError())
    end
end

function Base.convert(TN::Type{IntegerWithNaN{T,E}}, x::IntegerWithNaN) where {T,E}
    if !isnan(x)
        TN(convert(E, convert(T, x.encoded)))
    else
        intnan(T)
    end
end


Base.rem(x::IntegerWithNaN{T,E}, ::Type{<:Integer}) where {T,E} =
    rem(x.encoded, T)

Base.rem(x::Integer, TN::Type{IntegerWithNaN{T,E}}) where {T,E} =
    TN(rem(rem(x, T), E))

Base.rem(x::IntegerWithNaN, TN::Type{IntegerWithNaN{T,E}}) where {T,E} =
    TN(rem(x.encoded, E))




function withnan end
export withnan

Base.@pure withnan(::Type{Int8}) = IntegerWithNaN{Int8,Int32}
Base.@pure withnan(::Type{UInt8}) = IntegerWithNaN{UInt8,UInt32}
Base.@pure withnan(::Type{Int16}) = IntegerWithNaN{Int16,Int32}
Base.@pure withnan(::Type{UInt16}) = IntegerWithNaN{UInt16,UInt32}
Base.@pure withnan(::Type{Int32}) = IntegerWithNaN{Int32,Int32}
Base.@pure withnan(::Type{UInt32}) = IntegerWithNaN{UInt32,UInt32}
Base.@pure withnan(::Type{Int64}) = IntegerWithNaN{Int64,Int64}
Base.@pure withnan(::Type{UInt64}) = IntegerWithNaN{UInt64,UInt64}

withnan(x::Integer) = convert(withnan(typeof(x)), x)


function intnan end
export intnan

Base.@pure intnan(TN::Type{IntegerWithNaN{T,E}}) where {T,E} = TN(encode_nan(E))

intnan(T::Type{<:Integer}) = intnan(withnan(T))


Base.@pure encode_nan(E::Type{<:Integer}) = typemax(E)





