
(dap-register-debug-template
  "NetCoreDbg::Launch"
  (list :type "coreclr"
        :request "launch"
        :mode "launch"
        :program "./bin/Debug/net9.0/cspong.dll"
        :name "NetCoreDbg Launch"))
