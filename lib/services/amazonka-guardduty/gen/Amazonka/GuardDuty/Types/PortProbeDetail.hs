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
-- Module      : Amazonka.GuardDuty.Types.PortProbeDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.PortProbeDetail where

import qualified Amazonka.Core as Core
import Amazonka.GuardDuty.Types.LocalIpDetails
import Amazonka.GuardDuty.Types.LocalPortDetails
import Amazonka.GuardDuty.Types.RemoteIpDetails
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the port probe details.
--
-- /See:/ 'newPortProbeDetail' smart constructor.
data PortProbeDetail = PortProbeDetail'
  { -- | The remote IP information of the connection.
    remoteIpDetails :: Prelude.Maybe RemoteIpDetails,
    -- | The local IP information of the connection.
    localIpDetails :: Prelude.Maybe LocalIpDetails,
    -- | The local port information of the connection.
    localPortDetails :: Prelude.Maybe LocalPortDetails
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
-- 'remoteIpDetails', 'portProbeDetail_remoteIpDetails' - The remote IP information of the connection.
--
-- 'localIpDetails', 'portProbeDetail_localIpDetails' - The local IP information of the connection.
--
-- 'localPortDetails', 'portProbeDetail_localPortDetails' - The local port information of the connection.
newPortProbeDetail ::
  PortProbeDetail
newPortProbeDetail =
  PortProbeDetail'
    { remoteIpDetails = Prelude.Nothing,
      localIpDetails = Prelude.Nothing,
      localPortDetails = Prelude.Nothing
    }

-- | The remote IP information of the connection.
portProbeDetail_remoteIpDetails :: Lens.Lens' PortProbeDetail (Prelude.Maybe RemoteIpDetails)
portProbeDetail_remoteIpDetails = Lens.lens (\PortProbeDetail' {remoteIpDetails} -> remoteIpDetails) (\s@PortProbeDetail' {} a -> s {remoteIpDetails = a} :: PortProbeDetail)

-- | The local IP information of the connection.
portProbeDetail_localIpDetails :: Lens.Lens' PortProbeDetail (Prelude.Maybe LocalIpDetails)
portProbeDetail_localIpDetails = Lens.lens (\PortProbeDetail' {localIpDetails} -> localIpDetails) (\s@PortProbeDetail' {} a -> s {localIpDetails = a} :: PortProbeDetail)

-- | The local port information of the connection.
portProbeDetail_localPortDetails :: Lens.Lens' PortProbeDetail (Prelude.Maybe LocalPortDetails)
portProbeDetail_localPortDetails = Lens.lens (\PortProbeDetail' {localPortDetails} -> localPortDetails) (\s@PortProbeDetail' {} a -> s {localPortDetails = a} :: PortProbeDetail)

instance Core.FromJSON PortProbeDetail where
  parseJSON =
    Core.withObject
      "PortProbeDetail"
      ( \x ->
          PortProbeDetail'
            Prelude.<$> (x Core..:? "remoteIpDetails")
            Prelude.<*> (x Core..:? "localIpDetails")
            Prelude.<*> (x Core..:? "localPortDetails")
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
