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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterVpcSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterVpcSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A VPC security group that the cluster belongs to, if the cluster is in a
-- VPC.
--
-- /See:/ 'newAwsRedshiftClusterVpcSecurityGroup' smart constructor.
data AwsRedshiftClusterVpcSecurityGroup = AwsRedshiftClusterVpcSecurityGroup'
  { -- | The status of the VPC security group.
    status :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC security group.
    vpcSecurityGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterVpcSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsRedshiftClusterVpcSecurityGroup_status' - The status of the VPC security group.
--
-- 'vpcSecurityGroupId', 'awsRedshiftClusterVpcSecurityGroup_vpcSecurityGroupId' - The identifier of the VPC security group.
newAwsRedshiftClusterVpcSecurityGroup ::
  AwsRedshiftClusterVpcSecurityGroup
newAwsRedshiftClusterVpcSecurityGroup =
  AwsRedshiftClusterVpcSecurityGroup'
    { status =
        Prelude.Nothing,
      vpcSecurityGroupId = Prelude.Nothing
    }

-- | The status of the VPC security group.
awsRedshiftClusterVpcSecurityGroup_status :: Lens.Lens' AwsRedshiftClusterVpcSecurityGroup (Prelude.Maybe Prelude.Text)
awsRedshiftClusterVpcSecurityGroup_status = Lens.lens (\AwsRedshiftClusterVpcSecurityGroup' {status} -> status) (\s@AwsRedshiftClusterVpcSecurityGroup' {} a -> s {status = a} :: AwsRedshiftClusterVpcSecurityGroup)

-- | The identifier of the VPC security group.
awsRedshiftClusterVpcSecurityGroup_vpcSecurityGroupId :: Lens.Lens' AwsRedshiftClusterVpcSecurityGroup (Prelude.Maybe Prelude.Text)
awsRedshiftClusterVpcSecurityGroup_vpcSecurityGroupId = Lens.lens (\AwsRedshiftClusterVpcSecurityGroup' {vpcSecurityGroupId} -> vpcSecurityGroupId) (\s@AwsRedshiftClusterVpcSecurityGroup' {} a -> s {vpcSecurityGroupId = a} :: AwsRedshiftClusterVpcSecurityGroup)

instance
  Core.FromJSON
    AwsRedshiftClusterVpcSecurityGroup
  where
  parseJSON =
    Core.withObject
      "AwsRedshiftClusterVpcSecurityGroup"
      ( \x ->
          AwsRedshiftClusterVpcSecurityGroup'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "VpcSecurityGroupId")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterVpcSecurityGroup
  where
  hashWithSalt
    salt'
    AwsRedshiftClusterVpcSecurityGroup' {..} =
      salt' `Prelude.hashWithSalt` vpcSecurityGroupId
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsRedshiftClusterVpcSecurityGroup
  where
  rnf AwsRedshiftClusterVpcSecurityGroup' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf vpcSecurityGroupId

instance
  Core.ToJSON
    AwsRedshiftClusterVpcSecurityGroup
  where
  toJSON AwsRedshiftClusterVpcSecurityGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("VpcSecurityGroupId" Core..=)
              Prelude.<$> vpcSecurityGroupId
          ]
      )
