#=
zi:
- Julia version: 1.5.1
- Author: Patrizia Favaron
- Date: 2020-10-24
=#

using DifferentialEquations

function Zi(
	time_stamp::Array{DateTime,1},
	wt::Array{Float64,1},
	u_star::Array{Float64,1},
	H0::Array{Float64,1},
	L::Array{Float64,1},
	T::Array{Float64,1},
	lat::Float64,
	lon::Float64,
	zone::Int64,
	avg_time::Float64,
	zr::Float64 = 10.0,
	z0::Float64 = 0.023,
	h0_threshold::Float64 = 10,
	N::Float64 = 0.012,
	GB_initialization::string = "zero"
)

    #######################
    # Auxiliary functions #
    #######################

	g(z::Float64) = 3.0/(z+1) - 1.98e-3 + 2.27e-6*z

  	gry_bat(zi, p, t) = p[1]/p[5](zi) * 1/(zi^2/(1.4*zi-2*p[2]) + 8*p[3]^2*p[4]/(p[5](zi)*9.807*(1.2*zi-p[2])))

	###############
	# Actual code #
    ###############

    local pi = 3.1415926535

	# Coriolis parameter
	local f = 2.0*7.29e-5*sin(lat*pi/180.0)

	# Select useful data
	n   = length(time_stamp)
	Ta  = T + 273.15

	# Preallocate vectors (this will spare R a lot of behind-the-scenes work)
	zi             = Array{Float64,1}[undef, n]
	zi_convective  = Array{Float64,1}[undef, n]
	zi_stable      = Array{Float64,1}[undef, n]
	zi_neutral     = Array{Float64,1}[undef, n]
	zi_mechanical  = Array{Float64,1}[undef, n]
	zi_mech.stable = Array{Float64,1}[undef, n]

	# Classify situations based on turbulent flux of sensible heat; the "h0_threshold" value
	# is a non-negative value whose default, 0, excludes neutral cases. Sensible determination
	# of threshold should be made on a site basis.
	stable     = findall(H0 .< -h0_threshold)
	neutral    = findall(abs(H0) .<= h0_threshold)
	convective = findall(H0 .> h0_threshold)

	# Compute the main terms of diagnostic-only Zilitinkevich parameterization
	# and assemble them to equilibrium height equation. Once this has been found,
	# solve it and form stable part.
	for idx in stable
        local zi_s_1 = -0.125*(
				0.05 * N * Ta[idx] * u_star[idx]^2
				- 0.39228 * wt[idx]
				+ Ta[idx] * u_star[idx]^2 * sqrt(
					16.0 * f^2 + (
						0.05 * N - 0.39228 * wt[idx] / (Ta[idx] * u_star[idx]^2)
						+ 1.84213 * sqrt(abs(f * wt[idx] / Ta[idx]))
					)^2
				)
				+ 1.84213 * sqrt(abs(f * wt[idx] / Ta[idx]))
			)/(f^2 * Ta * u_star[idx])
        local zi_s_2 = 0.125*(
				- 0.05 * N * Ta[idx] * u_star[idx]^2
				+ 0.39228 * wt[idx]
				+ Ta[idx] * u_star[idx]^2 * sqrt(
					16.0 * f^2 + (
						0.05 * N - 0.39228 * wt[idx] / (Ta[idx] * u_star[idx]^2)
						+ 1.84213 * sqrt(abs(f * wt[idx] / Ta[idx]))
					)^2
				)
				- 1.84213 * sqrt(abs(f * wt[idx] / Ta[idx]))
			)/(f^2 * Ta * u_star[idx])
		zi_stable[idx] = max(zi_s_1, zi_s_2)
	end

	# Estimate the mechanical only mixing height using "u_star" parameterization.
	# Warning: the coefficient 1330 should in reality depend on Coriolis parameter;
	# ======== in this edition we don't care. In some next version, dependence will
	#          be made explicit.
	zi_mechanical = 1330 .* u_star

	# Compose the "mechanical basis" of mixing height by assuming Zilitinkevich
	# parameterization in stable hours, and mechanical "ustar" parameterization
	# on all neutral and convective situations.
	zi_mech_stable = zi_mechanical
	zi_mech_stable[stable] = zi_stable[stable]
	# Post-condition: "zi.mech.stable" is (or better, "should be") defined for all hours,
	# and not only the stable ones as pure Zilitinkevich method would.

	# Gryning-Batchvarova model, applied to all convective block of day. This
	# section is composed by two nested loops, an outer one iterating over days
	# and an inner one running GB equation on actual inner part.
	#
	# Locate the beginning day in data set
	local seconds_per_day = 24*3600
	local first_day = floor(time_stamp, Day)

	# Ensure first day is complete
	if time_stamp[1] != first_day
		 first_day = floor(min(time_stamp), Day)
	end

	# Locate the first day immediately after completion of data set, and count available days based on this.
	local last_day = ceil(max(time_stamp), Day)
	local num_days = round(last_day - first_day, Day)

	# Main (daily) loop
	for day_index in 1:num_days

		# Delimit current day
		local this_day_begin      = first_day + Dates.Day(day.index-1)
		local this_day_end        = this_day_begin + Dates.Day(1)
		local this_day            = (this_day_begin .<= time_stamp) .& (time_stamp .<= this_day_end)
		local this_day_index      = findall(this_day)
		local convective_this_day = findall((this_day_begin .<= time_stamp) .& (time_stamp .<= this_day_end) .& (H0 .> h0_threshold))

		# Check something convective exists in this day
		if length(convective_this_day) > 0

			# Delimit convective part of this day
			begin_convective = min(convective_this_day)
			end_convective   = max(convective_this_day)
			initial_value    = 0.

			# Actual Gryning-Batchvarova step
			for k in begin_convective:end_convective
				p         = [wt[k], L[k], u_star[k], Ta[k], g]
				zi0       = initial_value
				time_span = (0.0,60.0*avg_time)
				problem   = ODEProblem(gry_bat, zi0, time_span)
				solution  = solve(problem, Tsit5(), reltol=1.e-8, abstol=1.e-8)
				zi_convective[k] = solution(60.0*avg_time)
				initial_value    = zi_convective[k]
			end

		end

	end

******************<Patti_was_Here>*******************

	# Build final mixing height as maximum between "stable-mechanical" and convective, where the latter exists.
	zi <- zi.mech.stable
	for k in 1:n.data
		if !is.na(zi.convective[k])
			zi[k] <- max(c(zi.mech.stable[k], zi.convective[k]));
		end
	end

  return (zi, zi_stable, zi_mechanical, zi_convective)

end
