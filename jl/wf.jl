#!/Applications/Julia-1.5.app/Contents/Resources/julia/bin/julia

module WildFlower

    mutable struct Point
        x::Float64
        y::Float64
    end

    function Initialize()
        local θ = 2. * pi * rand(Float64)
        local ρ = rand(Float64)
        return Point(ρ*cos(θ), ρ*sin(θ))
    end

end


