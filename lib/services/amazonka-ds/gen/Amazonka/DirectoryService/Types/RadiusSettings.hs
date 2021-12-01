{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DirectoryService.Types.RadiusSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.RadiusSettings where

import qualified Amazonka.Core as Core
import Amazonka.DirectoryService.Types.RadiusAuthenticationProtocol
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a Remote Authentication Dial In User Service
-- (RADIUS) server.
--
-- /See:/ 'newRadiusSettings' smart constructor.
data RadiusSettings = RadiusSettings'
  { -- | Not currently used.
    displayLabel :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of times that communication with the RADIUS server is
    -- attempted.
    radiusRetries :: Prelude.Maybe Prelude.Natural,
    -- | The protocol specified for your RADIUS endpoints.
    authenticationProtocol :: Prelude.Maybe RadiusAuthenticationProtocol,
    -- | An array of strings that contains the fully qualified domain name (FQDN)
    -- or IP addresses of the RADIUS server endpoints, or the FQDN or IP
    -- addresses of your RADIUS server load balancer.
    radiusServers :: Prelude.Maybe [Prelude.Text],
    -- | Not currently used.
    useSameUsername :: Prelude.Maybe Prelude.Bool,
    -- | Required for enabling RADIUS on the directory.
    sharedSecret :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The amount of time, in seconds, to wait for the RADIUS server to
    -- respond.
    radiusTimeout :: Prelude.Maybe Prelude.Natural,
    -- | The port that your RADIUS server is using for communications. Your
    -- self-managed network must allow inbound traffic over this port from the
    -- Directory Service servers.
    radiusPort :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RadiusSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayLabel', 'radiusSettings_displayLabel' - Not currently used.
--
-- 'radiusRetries', 'radiusSettings_radiusRetries' - The maximum number of times that communication with the RADIUS server is
-- attempted.
--
-- 'authenticationProtocol', 'radiusSettings_authenticationProtocol' - The protocol specified for your RADIUS endpoints.
--
-- 'radiusServers', 'radiusSettings_radiusServers' - An array of strings that contains the fully qualified domain name (FQDN)
-- or IP addresses of the RADIUS server endpoints, or the FQDN or IP
-- addresses of your RADIUS server load balancer.
--
-- 'useSameUsername', 'radiusSettings_useSameUsername' - Not currently used.
--
-- 'sharedSecret', 'radiusSettings_sharedSecret' - Required for enabling RADIUS on the directory.
--
-- 'radiusTimeout', 'radiusSettings_radiusTimeout' - The amount of time, in seconds, to wait for the RADIUS server to
-- respond.
--
-- 'radiusPort', 'radiusSettings_radiusPort' - The port that your RADIUS server is using for communications. Your
-- self-managed network must allow inbound traffic over this port from the
-- Directory Service servers.
newRadiusSettings ::
  RadiusSettings
newRadiusSettings =
  RadiusSettings'
    { displayLabel = Prelude.Nothing,
      radiusRetries = Prelude.Nothing,
      authenticationProtocol = Prelude.Nothing,
      radiusServers = Prelude.Nothing,
      useSameUsername = Prelude.Nothing,
      sharedSecret = Prelude.Nothing,
      radiusTimeout = Prelude.Nothing,
      radiusPort = Prelude.Nothing
    }

-- | Not currently used.
radiusSettings_displayLabel :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Text)
radiusSettings_displayLabel = Lens.lens (\RadiusSettings' {displayLabel} -> displayLabel) (\s@RadiusSettings' {} a -> s {displayLabel = a} :: RadiusSettings)

-- | The maximum number of times that communication with the RADIUS server is
-- attempted.
radiusSettings_radiusRetries :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Natural)
radiusSettings_radiusRetries = Lens.lens (\RadiusSettings' {radiusRetries} -> radiusRetries) (\s@RadiusSettings' {} a -> s {radiusRetries = a} :: RadiusSettings)

-- | The protocol specified for your RADIUS endpoints.
radiusSettings_authenticationProtocol :: Lens.Lens' RadiusSettings (Prelude.Maybe RadiusAuthenticationProtocol)
radiusSettings_authenticationProtocol = Lens.lens (\RadiusSettings' {authenticationProtocol} -> authenticationProtocol) (\s@RadiusSettings' {} a -> s {authenticationProtocol = a} :: RadiusSettings)

-- | An array of strings that contains the fully qualified domain name (FQDN)
-- or IP addresses of the RADIUS server endpoints, or the FQDN or IP
-- addresses of your RADIUS server load balancer.
radiusSettings_radiusServers :: Lens.Lens' RadiusSettings (Prelude.Maybe [Prelude.Text])
radiusSettings_radiusServers = Lens.lens (\RadiusSettings' {radiusServers} -> radiusServers) (\s@RadiusSettings' {} a -> s {radiusServers = a} :: RadiusSettings) Prelude.. Lens.mapping Lens.coerced

-- | Not currently used.
radiusSettings_useSameUsername :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Bool)
radiusSettings_useSameUsername = Lens.lens (\RadiusSettings' {useSameUsername} -> useSameUsername) (\s@RadiusSettings' {} a -> s {useSameUsername = a} :: RadiusSettings)

-- | Required for enabling RADIUS on the directory.
radiusSettings_sharedSecret :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Text)
radiusSettings_sharedSecret = Lens.lens (\RadiusSettings' {sharedSecret} -> sharedSecret) (\s@RadiusSettings' {} a -> s {sharedSecret = a} :: RadiusSettings) Prelude.. Lens.mapping Core._Sensitive

-- | The amount of time, in seconds, to wait for the RADIUS server to
-- respond.
radiusSettings_radiusTimeout :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Natural)
radiusSettings_radiusTimeout = Lens.lens (\RadiusSettings' {radiusTimeout} -> radiusTimeout) (\s@RadiusSettings' {} a -> s {radiusTimeout = a} :: RadiusSettings)

-- | The port that your RADIUS server is using for communications. Your
-- self-managed network must allow inbound traffic over this port from the
-- Directory Service servers.
radiusSettings_radiusPort :: Lens.Lens' RadiusSettings (Prelude.Maybe Prelude.Natural)
radiusSettings_radiusPort = Lens.lens (\RadiusSettings' {radiusPort} -> radiusPort) (\s@RadiusSettings' {} a -> s {radiusPort = a} :: RadiusSettings)

instance Core.FromJSON RadiusSettings where
  parseJSON =
    Core.withObject
      "RadiusSettings"
      ( \x ->
          RadiusSettings'
            Prelude.<$> (x Core..:? "DisplayLabel")
            Prelude.<*> (x Core..:? "RadiusRetries")
            Prelude.<*> (x Core..:? "AuthenticationProtocol")
            Prelude.<*> (x Core..:? "RadiusServers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "UseSameUsername")
            Prelude.<*> (x Core..:? "SharedSecret")
            Prelude.<*> (x Core..:? "RadiusTimeout")
            Prelude.<*> (x Core..:? "RadiusPort")
      )

instance Prelude.Hashable RadiusSettings where
  hashWithSalt salt' RadiusSettings' {..} =
    salt' `Prelude.hashWithSalt` radiusPort
      `Prelude.hashWithSalt` radiusTimeout
      `Prelude.hashWithSalt` sharedSecret
      `Prelude.hashWithSalt` useSameUsername
      `Prelude.hashWithSalt` radiusServers
      `Prelude.hashWithSalt` authenticationProtocol
      `Prelude.hashWithSalt` radiusRetries
      `Prelude.hashWithSalt` displayLabel

instance Prelude.NFData RadiusSettings where
  rnf RadiusSettings' {..} =
    Prelude.rnf displayLabel
      `Prelude.seq` Prelude.rnf radiusPort
      `Prelude.seq` Prelude.rnf radiusTimeout
      `Prelude.seq` Prelude.rnf sharedSecret
      `Prelude.seq` Prelude.rnf useSameUsername
      `Prelude.seq` Prelude.rnf radiusServers
      `Prelude.seq` Prelude.rnf authenticationProtocol
      `Prelude.seq` Prelude.rnf radiusRetries

instance Core.ToJSON RadiusSettings where
  toJSON RadiusSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DisplayLabel" Core..=) Prelude.<$> displayLabel,
            ("RadiusRetries" Core..=) Prelude.<$> radiusRetries,
            ("AuthenticationProtocol" Core..=)
              Prelude.<$> authenticationProtocol,
            ("RadiusServers" Core..=) Prelude.<$> radiusServers,
            ("UseSameUsername" Core..=)
              Prelude.<$> useSameUsername,
            ("SharedSecret" Core..=) Prelude.<$> sharedSecret,
            ("RadiusTimeout" Core..=) Prelude.<$> radiusTimeout,
            ("RadiusPort" Core..=) Prelude.<$> radiusPort
          ]
      )
