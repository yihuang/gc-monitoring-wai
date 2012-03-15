import Network.Wai
import Network.Wai.Application.Monitoring (monitorGC)
import Network.Wai.Handler.Warp (run)

main = run 3000 monitorGC
