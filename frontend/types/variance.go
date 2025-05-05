package types

type varianceInfo struct {
	covariant, contravariant bool
}

var (
	varianceBivaraint     = varianceInfo{covariant: true, contravariant: true}
	varianceCovariant     = varianceInfo{covariant: true}
	varianceContravariant = varianceInfo{contravariant: true}
	varianceInvariant     = varianceInfo{}
)
