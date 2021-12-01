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
-- Module      : Amazonka.RDS.Types.DBProxyTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBProxyTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.TargetHealth
import Amazonka.RDS.Types.TargetRole
import Amazonka.RDS.Types.TargetType

-- | Contains the details for an RDS Proxy target. It represents an RDS DB
-- instance or Aurora DB cluster that the proxy can connect to. One or more
-- targets are associated with an RDS Proxy target group.
--
-- This data type is used as a response element in the
-- @DescribeDBProxyTargets@ action.
--
-- /See:/ 'newDBProxyTarget' smart constructor.
data DBProxyTarget = DBProxyTarget'
  { -- | The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB
    -- cluster.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the connection health of the RDS Proxy target.
    targetHealth :: Prelude.Maybe TargetHealth,
    -- | The DB cluster identifier when the target represents an Aurora DB
    -- cluster. This field is blank when the target represents an RDS DB
    -- instance.
    trackedClusterId :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the target of the proxy can be used for
    -- read\/write or read-only operations.
    role' :: Prelude.Maybe TargetRole,
    -- | The identifier representing the target. It can be the instance
    -- identifier for an RDS DB instance, or the cluster identifier for an
    -- Aurora DB cluster.
    rdsResourceId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the kind of database, such as an RDS DB instance or an Aurora
    -- DB cluster, that the target represents.
    type' :: Prelude.Maybe TargetType,
    -- | The writer endpoint for the RDS DB instance or Aurora DB cluster.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The port that the RDS Proxy uses to connect to the target RDS DB
    -- instance or Aurora DB cluster.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBProxyTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetArn', 'dbProxyTarget_targetArn' - The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB
-- cluster.
--
-- 'targetHealth', 'dbProxyTarget_targetHealth' - Information about the connection health of the RDS Proxy target.
--
-- 'trackedClusterId', 'dbProxyTarget_trackedClusterId' - The DB cluster identifier when the target represents an Aurora DB
-- cluster. This field is blank when the target represents an RDS DB
-- instance.
--
-- 'role'', 'dbProxyTarget_role' - A value that indicates whether the target of the proxy can be used for
-- read\/write or read-only operations.
--
-- 'rdsResourceId', 'dbProxyTarget_rdsResourceId' - The identifier representing the target. It can be the instance
-- identifier for an RDS DB instance, or the cluster identifier for an
-- Aurora DB cluster.
--
-- 'type'', 'dbProxyTarget_type' - Specifies the kind of database, such as an RDS DB instance or an Aurora
-- DB cluster, that the target represents.
--
-- 'endpoint', 'dbProxyTarget_endpoint' - The writer endpoint for the RDS DB instance or Aurora DB cluster.
--
-- 'port', 'dbProxyTarget_port' - The port that the RDS Proxy uses to connect to the target RDS DB
-- instance or Aurora DB cluster.
newDBProxyTarget ::
  DBProxyTarget
newDBProxyTarget =
  DBProxyTarget'
    { targetArn = Prelude.Nothing,
      targetHealth = Prelude.Nothing,
      trackedClusterId = Prelude.Nothing,
      role' = Prelude.Nothing,
      rdsResourceId = Prelude.Nothing,
      type' = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB
-- cluster.
dbProxyTarget_targetArn :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_targetArn = Lens.lens (\DBProxyTarget' {targetArn} -> targetArn) (\s@DBProxyTarget' {} a -> s {targetArn = a} :: DBProxyTarget)

-- | Information about the connection health of the RDS Proxy target.
dbProxyTarget_targetHealth :: Lens.Lens' DBProxyTarget (Prelude.Maybe TargetHealth)
dbProxyTarget_targetHealth = Lens.lens (\DBProxyTarget' {targetHealth} -> targetHealth) (\s@DBProxyTarget' {} a -> s {targetHealth = a} :: DBProxyTarget)

-- | The DB cluster identifier when the target represents an Aurora DB
-- cluster. This field is blank when the target represents an RDS DB
-- instance.
dbProxyTarget_trackedClusterId :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_trackedClusterId = Lens.lens (\DBProxyTarget' {trackedClusterId} -> trackedClusterId) (\s@DBProxyTarget' {} a -> s {trackedClusterId = a} :: DBProxyTarget)

-- | A value that indicates whether the target of the proxy can be used for
-- read\/write or read-only operations.
dbProxyTarget_role :: Lens.Lens' DBProxyTarget (Prelude.Maybe TargetRole)
dbProxyTarget_role = Lens.lens (\DBProxyTarget' {role'} -> role') (\s@DBProxyTarget' {} a -> s {role' = a} :: DBProxyTarget)

-- | The identifier representing the target. It can be the instance
-- identifier for an RDS DB instance, or the cluster identifier for an
-- Aurora DB cluster.
dbProxyTarget_rdsResourceId :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_rdsResourceId = Lens.lens (\DBProxyTarget' {rdsResourceId} -> rdsResourceId) (\s@DBProxyTarget' {} a -> s {rdsResourceId = a} :: DBProxyTarget)

-- | Specifies the kind of database, such as an RDS DB instance or an Aurora
-- DB cluster, that the target represents.
dbProxyTarget_type :: Lens.Lens' DBProxyTarget (Prelude.Maybe TargetType)
dbProxyTarget_type = Lens.lens (\DBProxyTarget' {type'} -> type') (\s@DBProxyTarget' {} a -> s {type' = a} :: DBProxyTarget)

-- | The writer endpoint for the RDS DB instance or Aurora DB cluster.
dbProxyTarget_endpoint :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_endpoint = Lens.lens (\DBProxyTarget' {endpoint} -> endpoint) (\s@DBProxyTarget' {} a -> s {endpoint = a} :: DBProxyTarget)

-- | The port that the RDS Proxy uses to connect to the target RDS DB
-- instance or Aurora DB cluster.
dbProxyTarget_port :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Int)
dbProxyTarget_port = Lens.lens (\DBProxyTarget' {port} -> port) (\s@DBProxyTarget' {} a -> s {port = a} :: DBProxyTarget)

instance Core.FromXML DBProxyTarget where
  parseXML x =
    DBProxyTarget'
      Prelude.<$> (x Core..@? "TargetArn")
      Prelude.<*> (x Core..@? "TargetHealth")
      Prelude.<*> (x Core..@? "TrackedClusterId")
      Prelude.<*> (x Core..@? "Role")
      Prelude.<*> (x Core..@? "RdsResourceId")
      Prelude.<*> (x Core..@? "Type")
      Prelude.<*> (x Core..@? "Endpoint")
      Prelude.<*> (x Core..@? "Port")

instance Prelude.Hashable DBProxyTarget where
  hashWithSalt salt' DBProxyTarget' {..} =
    salt' `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` rdsResourceId
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` trackedClusterId
      `Prelude.hashWithSalt` targetHealth
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData DBProxyTarget where
  rnf DBProxyTarget' {..} =
    Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf rdsResourceId
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf trackedClusterId
      `Prelude.seq` Prelude.rnf targetHealth
