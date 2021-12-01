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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbInstanceVpcSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbInstanceVpcSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A VPC security groups that the DB instance belongs to.
--
-- /See:/ 'newAwsRdsDbInstanceVpcSecurityGroup' smart constructor.
data AwsRdsDbInstanceVpcSecurityGroup = AwsRdsDbInstanceVpcSecurityGroup'
  { -- | The status of the VPC security group.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the VPC security group.
    vpcSecurityGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbInstanceVpcSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsRdsDbInstanceVpcSecurityGroup_status' - The status of the VPC security group.
--
-- 'vpcSecurityGroupId', 'awsRdsDbInstanceVpcSecurityGroup_vpcSecurityGroupId' - The name of the VPC security group.
newAwsRdsDbInstanceVpcSecurityGroup ::
  AwsRdsDbInstanceVpcSecurityGroup
newAwsRdsDbInstanceVpcSecurityGroup =
  AwsRdsDbInstanceVpcSecurityGroup'
    { status =
        Prelude.Nothing,
      vpcSecurityGroupId = Prelude.Nothing
    }

-- | The status of the VPC security group.
awsRdsDbInstanceVpcSecurityGroup_status :: Lens.Lens' AwsRdsDbInstanceVpcSecurityGroup (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceVpcSecurityGroup_status = Lens.lens (\AwsRdsDbInstanceVpcSecurityGroup' {status} -> status) (\s@AwsRdsDbInstanceVpcSecurityGroup' {} a -> s {status = a} :: AwsRdsDbInstanceVpcSecurityGroup)

-- | The name of the VPC security group.
awsRdsDbInstanceVpcSecurityGroup_vpcSecurityGroupId :: Lens.Lens' AwsRdsDbInstanceVpcSecurityGroup (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceVpcSecurityGroup_vpcSecurityGroupId = Lens.lens (\AwsRdsDbInstanceVpcSecurityGroup' {vpcSecurityGroupId} -> vpcSecurityGroupId) (\s@AwsRdsDbInstanceVpcSecurityGroup' {} a -> s {vpcSecurityGroupId = a} :: AwsRdsDbInstanceVpcSecurityGroup)

instance
  Core.FromJSON
    AwsRdsDbInstanceVpcSecurityGroup
  where
  parseJSON =
    Core.withObject
      "AwsRdsDbInstanceVpcSecurityGroup"
      ( \x ->
          AwsRdsDbInstanceVpcSecurityGroup'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "VpcSecurityGroupId")
      )

instance
  Prelude.Hashable
    AwsRdsDbInstanceVpcSecurityGroup
  where
  hashWithSalt
    salt'
    AwsRdsDbInstanceVpcSecurityGroup' {..} =
      salt' `Prelude.hashWithSalt` vpcSecurityGroupId
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsRdsDbInstanceVpcSecurityGroup
  where
  rnf AwsRdsDbInstanceVpcSecurityGroup' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf vpcSecurityGroupId

instance Core.ToJSON AwsRdsDbInstanceVpcSecurityGroup where
  toJSON AwsRdsDbInstanceVpcSecurityGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("VpcSecurityGroupId" Core..=)
              Prelude.<$> vpcSecurityGroupId
          ]
      )
