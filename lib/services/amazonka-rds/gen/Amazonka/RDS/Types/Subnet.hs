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
-- Module      : Amazonka.RDS.Types.Subnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.Subnet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.AvailabilityZone
import Amazonka.RDS.Types.Outpost

-- | This data type is used as a response element for the
-- @DescribeDBSubnetGroups@ operation.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The status of the subnet.
    subnetStatus :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the subnet.
    subnetIdentifier :: Prelude.Maybe Prelude.Text,
    subnetAvailabilityZone :: Prelude.Maybe AvailabilityZone,
    -- | If the subnet is associated with an Outpost, this value specifies the
    -- Outpost.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
    -- in the /Amazon RDS User Guide./
    subnetOutpost :: Prelude.Maybe Outpost
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Subnet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetStatus', 'subnet_subnetStatus' - The status of the subnet.
--
-- 'subnetIdentifier', 'subnet_subnetIdentifier' - The identifier of the subnet.
--
-- 'subnetAvailabilityZone', 'subnet_subnetAvailabilityZone' - Undocumented member.
--
-- 'subnetOutpost', 'subnet_subnetOutpost' - If the subnet is associated with an Outpost, this value specifies the
-- Outpost.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide./
newSubnet ::
  Subnet
newSubnet =
  Subnet'
    { subnetStatus = Prelude.Nothing,
      subnetIdentifier = Prelude.Nothing,
      subnetAvailabilityZone = Prelude.Nothing,
      subnetOutpost = Prelude.Nothing
    }

-- | The status of the subnet.
subnet_subnetStatus :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetStatus = Lens.lens (\Subnet' {subnetStatus} -> subnetStatus) (\s@Subnet' {} a -> s {subnetStatus = a} :: Subnet)

-- | The identifier of the subnet.
subnet_subnetIdentifier :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetIdentifier = Lens.lens (\Subnet' {subnetIdentifier} -> subnetIdentifier) (\s@Subnet' {} a -> s {subnetIdentifier = a} :: Subnet)

-- | Undocumented member.
subnet_subnetAvailabilityZone :: Lens.Lens' Subnet (Prelude.Maybe AvailabilityZone)
subnet_subnetAvailabilityZone = Lens.lens (\Subnet' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@Subnet' {} a -> s {subnetAvailabilityZone = a} :: Subnet)

-- | If the subnet is associated with an Outpost, this value specifies the
-- Outpost.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide./
subnet_subnetOutpost :: Lens.Lens' Subnet (Prelude.Maybe Outpost)
subnet_subnetOutpost = Lens.lens (\Subnet' {subnetOutpost} -> subnetOutpost) (\s@Subnet' {} a -> s {subnetOutpost = a} :: Subnet)

instance Core.FromXML Subnet where
  parseXML x =
    Subnet'
      Prelude.<$> (x Core..@? "SubnetStatus")
      Prelude.<*> (x Core..@? "SubnetIdentifier")
      Prelude.<*> (x Core..@? "SubnetAvailabilityZone")
      Prelude.<*> (x Core..@? "SubnetOutpost")

instance Prelude.Hashable Subnet where
  hashWithSalt salt' Subnet' {..} =
    salt' `Prelude.hashWithSalt` subnetOutpost
      `Prelude.hashWithSalt` subnetAvailabilityZone
      `Prelude.hashWithSalt` subnetIdentifier
      `Prelude.hashWithSalt` subnetStatus

instance Prelude.NFData Subnet where
  rnf Subnet' {..} =
    Prelude.rnf subnetStatus
      `Prelude.seq` Prelude.rnf subnetOutpost
      `Prelude.seq` Prelude.rnf subnetAvailabilityZone
      `Prelude.seq` Prelude.rnf subnetIdentifier
