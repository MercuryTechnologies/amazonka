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
-- Module      : Amazonka.DirectoryService.Types.IpRouteInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.IpRouteInfo where

import qualified Amazonka.Core as Core
import Amazonka.DirectoryService.Types.IpRouteStatusMsg
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about one or more IP address blocks.
--
-- /See:/ 'newIpRouteInfo' smart constructor.
data IpRouteInfo = IpRouteInfo'
  { -- | Identifier (ID) of the directory associated with the IP addresses.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The reason for the IpRouteStatusMsg.
    ipRouteStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time the address block was added to the directory.
    addedDateTime :: Prelude.Maybe Core.POSIX,
    -- | IP address block in the IpRoute.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | The status of the IP address block.
    ipRouteStatusMsg :: Prelude.Maybe IpRouteStatusMsg,
    -- | Description of the IpRouteInfo.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpRouteInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'ipRouteInfo_directoryId' - Identifier (ID) of the directory associated with the IP addresses.
--
-- 'ipRouteStatusReason', 'ipRouteInfo_ipRouteStatusReason' - The reason for the IpRouteStatusMsg.
--
-- 'addedDateTime', 'ipRouteInfo_addedDateTime' - The date and time the address block was added to the directory.
--
-- 'cidrIp', 'ipRouteInfo_cidrIp' - IP address block in the IpRoute.
--
-- 'ipRouteStatusMsg', 'ipRouteInfo_ipRouteStatusMsg' - The status of the IP address block.
--
-- 'description', 'ipRouteInfo_description' - Description of the IpRouteInfo.
newIpRouteInfo ::
  IpRouteInfo
newIpRouteInfo =
  IpRouteInfo'
    { directoryId = Prelude.Nothing,
      ipRouteStatusReason = Prelude.Nothing,
      addedDateTime = Prelude.Nothing,
      cidrIp = Prelude.Nothing,
      ipRouteStatusMsg = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Identifier (ID) of the directory associated with the IP addresses.
ipRouteInfo_directoryId :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_directoryId = Lens.lens (\IpRouteInfo' {directoryId} -> directoryId) (\s@IpRouteInfo' {} a -> s {directoryId = a} :: IpRouteInfo)

-- | The reason for the IpRouteStatusMsg.
ipRouteInfo_ipRouteStatusReason :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_ipRouteStatusReason = Lens.lens (\IpRouteInfo' {ipRouteStatusReason} -> ipRouteStatusReason) (\s@IpRouteInfo' {} a -> s {ipRouteStatusReason = a} :: IpRouteInfo)

-- | The date and time the address block was added to the directory.
ipRouteInfo_addedDateTime :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.UTCTime)
ipRouteInfo_addedDateTime = Lens.lens (\IpRouteInfo' {addedDateTime} -> addedDateTime) (\s@IpRouteInfo' {} a -> s {addedDateTime = a} :: IpRouteInfo) Prelude.. Lens.mapping Core._Time

-- | IP address block in the IpRoute.
ipRouteInfo_cidrIp :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_cidrIp = Lens.lens (\IpRouteInfo' {cidrIp} -> cidrIp) (\s@IpRouteInfo' {} a -> s {cidrIp = a} :: IpRouteInfo)

-- | The status of the IP address block.
ipRouteInfo_ipRouteStatusMsg :: Lens.Lens' IpRouteInfo (Prelude.Maybe IpRouteStatusMsg)
ipRouteInfo_ipRouteStatusMsg = Lens.lens (\IpRouteInfo' {ipRouteStatusMsg} -> ipRouteStatusMsg) (\s@IpRouteInfo' {} a -> s {ipRouteStatusMsg = a} :: IpRouteInfo)

-- | Description of the IpRouteInfo.
ipRouteInfo_description :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_description = Lens.lens (\IpRouteInfo' {description} -> description) (\s@IpRouteInfo' {} a -> s {description = a} :: IpRouteInfo)

instance Core.FromJSON IpRouteInfo where
  parseJSON =
    Core.withObject
      "IpRouteInfo"
      ( \x ->
          IpRouteInfo'
            Prelude.<$> (x Core..:? "DirectoryId")
            Prelude.<*> (x Core..:? "IpRouteStatusReason")
            Prelude.<*> (x Core..:? "AddedDateTime")
            Prelude.<*> (x Core..:? "CidrIp")
            Prelude.<*> (x Core..:? "IpRouteStatusMsg")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable IpRouteInfo where
  hashWithSalt salt' IpRouteInfo' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ipRouteStatusMsg
      `Prelude.hashWithSalt` cidrIp
      `Prelude.hashWithSalt` addedDateTime
      `Prelude.hashWithSalt` ipRouteStatusReason
      `Prelude.hashWithSalt` directoryId

instance Prelude.NFData IpRouteInfo where
  rnf IpRouteInfo' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ipRouteStatusMsg
      `Prelude.seq` Prelude.rnf cidrIp
      `Prelude.seq` Prelude.rnf addedDateTime
      `Prelude.seq` Prelude.rnf ipRouteStatusReason
