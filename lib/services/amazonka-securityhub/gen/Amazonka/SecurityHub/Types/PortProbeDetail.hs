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
-- Module      : Amazonka.SecurityHub.Types.PortProbeDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.PortProbeDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.ActionLocalIpDetails
import Amazonka.SecurityHub.Types.ActionLocalPortDetails
import Amazonka.SecurityHub.Types.ActionRemoteIpDetails

-- | A port scan that was part of the port probe. For each scan,
-- PortProbeDetails provides information about the local IP address and
-- port that were scanned, and the remote IP address that the scan
-- originated from.
--
-- /See:/ 'newPortProbeDetail' smart constructor.
data PortProbeDetail = PortProbeDetail'
  { -- | Provides information about the remote IP address that performed the
    -- scan.
    remoteIpDetails :: Prelude.Maybe ActionRemoteIpDetails,
    -- | Provides information about the IP address where the scanned port is
    -- located.
    localIpDetails :: Prelude.Maybe ActionLocalIpDetails,
    -- | Provides information about the port that was scanned.
    localPortDetails :: Prelude.Maybe ActionLocalPortDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortProbeDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteIpDetails', 'portProbeDetail_remoteIpDetails' - Provides information about the remote IP address that performed the
-- scan.
--
-- 'localIpDetails', 'portProbeDetail_localIpDetails' - Provides information about the IP address where the scanned port is
-- located.
--
-- 'localPortDetails', 'portProbeDetail_localPortDetails' - Provides information about the port that was scanned.
newPortProbeDetail ::
  PortProbeDetail
newPortProbeDetail =
  PortProbeDetail'
    { remoteIpDetails = Prelude.Nothing,
      localIpDetails = Prelude.Nothing,
      localPortDetails = Prelude.Nothing
    }

-- | Provides information about the remote IP address that performed the
-- scan.
portProbeDetail_remoteIpDetails :: Lens.Lens' PortProbeDetail (Prelude.Maybe ActionRemoteIpDetails)
portProbeDetail_remoteIpDetails = Lens.lens (\PortProbeDetail' {remoteIpDetails} -> remoteIpDetails) (\s@PortProbeDetail' {} a -> s {remoteIpDetails = a} :: PortProbeDetail)

-- | Provides information about the IP address where the scanned port is
-- located.
portProbeDetail_localIpDetails :: Lens.Lens' PortProbeDetail (Prelude.Maybe ActionLocalIpDetails)
portProbeDetail_localIpDetails = Lens.lens (\PortProbeDetail' {localIpDetails} -> localIpDetails) (\s@PortProbeDetail' {} a -> s {localIpDetails = a} :: PortProbeDetail)

-- | Provides information about the port that was scanned.
portProbeDetail_localPortDetails :: Lens.Lens' PortProbeDetail (Prelude.Maybe ActionLocalPortDetails)
portProbeDetail_localPortDetails = Lens.lens (\PortProbeDetail' {localPortDetails} -> localPortDetails) (\s@PortProbeDetail' {} a -> s {localPortDetails = a} :: PortProbeDetail)

instance Core.FromJSON PortProbeDetail where
  parseJSON =
    Core.withObject
      "PortProbeDetail"
      ( \x ->
          PortProbeDetail'
            Prelude.<$> (x Core..:? "RemoteIpDetails")
            Prelude.<*> (x Core..:? "LocalIpDetails")
            Prelude.<*> (x Core..:? "LocalPortDetails")
      )

instance Prelude.Hashable PortProbeDetail where
  hashWithSalt salt' PortProbeDetail' {..} =
    salt' `Prelude.hashWithSalt` localPortDetails
      `Prelude.hashWithSalt` localIpDetails
      `Prelude.hashWithSalt` remoteIpDetails

instance Prelude.NFData PortProbeDetail where
  rnf PortProbeDetail' {..} =
    Prelude.rnf remoteIpDetails
      `Prelude.seq` Prelude.rnf localPortDetails
      `Prelude.seq` Prelude.rnf localIpDetails

instance Core.ToJSON PortProbeDetail where
  toJSON PortProbeDetail' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RemoteIpDetails" Core..=)
              Prelude.<$> remoteIpDetails,
            ("LocalIpDetails" Core..=)
              Prelude.<$> localIpDetails,
            ("LocalPortDetails" Core..=)
              Prelude.<$> localPortDetails
          ]
      )
