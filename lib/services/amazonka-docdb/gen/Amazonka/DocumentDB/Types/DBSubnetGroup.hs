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
-- Module      : Amazonka.DocumentDB.Types.DBSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.DBSubnetGroup where

import qualified Amazonka.Core as Core
import Amazonka.DocumentDB.Types.Subnet
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about a subnet group.
--
-- /See:/ 'newDBSubnetGroup' smart constructor.
data DBSubnetGroup = DBSubnetGroup'
  { -- | The name of the subnet group.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | Provides the virtual private cloud (VPC) ID of the subnet group.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about one or more subnets within a subnet group.
    subnets :: Prelude.Maybe [Subnet],
    -- | Provides the description of the subnet group.
    dbSubnetGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the DB subnet group.
    dbSubnetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Provides the status of the subnet group.
    subnetGroupStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSubnetGroupName', 'dbSubnetGroup_dbSubnetGroupName' - The name of the subnet group.
--
-- 'vpcId', 'dbSubnetGroup_vpcId' - Provides the virtual private cloud (VPC) ID of the subnet group.
--
-- 'subnets', 'dbSubnetGroup_subnets' - Detailed information about one or more subnets within a subnet group.
--
-- 'dbSubnetGroupDescription', 'dbSubnetGroup_dbSubnetGroupDescription' - Provides the description of the subnet group.
--
-- 'dbSubnetGroupArn', 'dbSubnetGroup_dbSubnetGroupArn' - The Amazon Resource Name (ARN) for the DB subnet group.
--
-- 'subnetGroupStatus', 'dbSubnetGroup_subnetGroupStatus' - Provides the status of the subnet group.
newDBSubnetGroup ::
  DBSubnetGroup
newDBSubnetGroup =
  DBSubnetGroup'
    { dbSubnetGroupName = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      subnets = Prelude.Nothing,
      dbSubnetGroupDescription = Prelude.Nothing,
      dbSubnetGroupArn = Prelude.Nothing,
      subnetGroupStatus = Prelude.Nothing
    }

-- | The name of the subnet group.
dbSubnetGroup_dbSubnetGroupName :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_dbSubnetGroupName = Lens.lens (\DBSubnetGroup' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupName = a} :: DBSubnetGroup)

-- | Provides the virtual private cloud (VPC) ID of the subnet group.
dbSubnetGroup_vpcId :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_vpcId = Lens.lens (\DBSubnetGroup' {vpcId} -> vpcId) (\s@DBSubnetGroup' {} a -> s {vpcId = a} :: DBSubnetGroup)

-- | Detailed information about one or more subnets within a subnet group.
dbSubnetGroup_subnets :: Lens.Lens' DBSubnetGroup (Prelude.Maybe [Subnet])
dbSubnetGroup_subnets = Lens.lens (\DBSubnetGroup' {subnets} -> subnets) (\s@DBSubnetGroup' {} a -> s {subnets = a} :: DBSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | Provides the description of the subnet group.
dbSubnetGroup_dbSubnetGroupDescription :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_dbSubnetGroupDescription = Lens.lens (\DBSubnetGroup' {dbSubnetGroupDescription} -> dbSubnetGroupDescription) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupDescription = a} :: DBSubnetGroup)

-- | The Amazon Resource Name (ARN) for the DB subnet group.
dbSubnetGroup_dbSubnetGroupArn :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_dbSubnetGroupArn = Lens.lens (\DBSubnetGroup' {dbSubnetGroupArn} -> dbSubnetGroupArn) (\s@DBSubnetGroup' {} a -> s {dbSubnetGroupArn = a} :: DBSubnetGroup)

-- | Provides the status of the subnet group.
dbSubnetGroup_subnetGroupStatus :: Lens.Lens' DBSubnetGroup (Prelude.Maybe Prelude.Text)
dbSubnetGroup_subnetGroupStatus = Lens.lens (\DBSubnetGroup' {subnetGroupStatus} -> subnetGroupStatus) (\s@DBSubnetGroup' {} a -> s {subnetGroupStatus = a} :: DBSubnetGroup)

instance Core.FromXML DBSubnetGroup where
  parseXML x =
    DBSubnetGroup'
      Prelude.<$> (x Core..@? "DBSubnetGroupName")
      Prelude.<*> (x Core..@? "VpcId")
      Prelude.<*> ( x Core..@? "Subnets" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Subnet")
                  )
      Prelude.<*> (x Core..@? "DBSubnetGroupDescription")
      Prelude.<*> (x Core..@? "DBSubnetGroupArn")
      Prelude.<*> (x Core..@? "SubnetGroupStatus")

instance Prelude.Hashable DBSubnetGroup where
  hashWithSalt salt' DBSubnetGroup' {..} =
    salt' `Prelude.hashWithSalt` subnetGroupStatus
      `Prelude.hashWithSalt` dbSubnetGroupArn
      `Prelude.hashWithSalt` dbSubnetGroupDescription
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` dbSubnetGroupName

instance Prelude.NFData DBSubnetGroup where
  rnf DBSubnetGroup' {..} =
    Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf subnetGroupStatus
      `Prelude.seq` Prelude.rnf dbSubnetGroupArn
      `Prelude.seq` Prelude.rnf dbSubnetGroupDescription
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf vpcId
