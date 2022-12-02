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
-- Module      : Amazonka.DocumentDB.Types.DBClusterRole
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.DBClusterRole where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Identity and Access Management (IAM) role that is
-- associated with a cluster.
--
-- /See:/ 'newDBClusterRole' smart constructor.
data DBClusterRole = DBClusterRole'
  { -- | The Amazon Resource Name (ARN) of the IAMrole that is associated with
    -- the DB cluster.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Describes the state of association between the IAMrole and the cluster.
    -- The @Status@ property returns one of the following values:
    --
    -- -   @ACTIVE@ - The IAMrole ARN is associated with the cluster and can be
    --     used to access other Amazon Web Services services on your behalf.
    --
    -- -   @PENDING@ - The IAMrole ARN is being associated with the cluster.
    --
    -- -   @INVALID@ - The IAMrole ARN is associated with the cluster, but the
    --     cluster cannot assume the IAMrole to access other Amazon Web
    --     Services services on your behalf.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBClusterRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'dbClusterRole_roleArn' - The Amazon Resource Name (ARN) of the IAMrole that is associated with
-- the DB cluster.
--
-- 'status', 'dbClusterRole_status' - Describes the state of association between the IAMrole and the cluster.
-- The @Status@ property returns one of the following values:
--
-- -   @ACTIVE@ - The IAMrole ARN is associated with the cluster and can be
--     used to access other Amazon Web Services services on your behalf.
--
-- -   @PENDING@ - The IAMrole ARN is being associated with the cluster.
--
-- -   @INVALID@ - The IAMrole ARN is associated with the cluster, but the
--     cluster cannot assume the IAMrole to access other Amazon Web
--     Services services on your behalf.
newDBClusterRole ::
  DBClusterRole
newDBClusterRole =
  DBClusterRole'
    { roleArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAMrole that is associated with
-- the DB cluster.
dbClusterRole_roleArn :: Lens.Lens' DBClusterRole (Prelude.Maybe Prelude.Text)
dbClusterRole_roleArn = Lens.lens (\DBClusterRole' {roleArn} -> roleArn) (\s@DBClusterRole' {} a -> s {roleArn = a} :: DBClusterRole)

-- | Describes the state of association between the IAMrole and the cluster.
-- The @Status@ property returns one of the following values:
--
-- -   @ACTIVE@ - The IAMrole ARN is associated with the cluster and can be
--     used to access other Amazon Web Services services on your behalf.
--
-- -   @PENDING@ - The IAMrole ARN is being associated with the cluster.
--
-- -   @INVALID@ - The IAMrole ARN is associated with the cluster, but the
--     cluster cannot assume the IAMrole to access other Amazon Web
--     Services services on your behalf.
dbClusterRole_status :: Lens.Lens' DBClusterRole (Prelude.Maybe Prelude.Text)
dbClusterRole_status = Lens.lens (\DBClusterRole' {status} -> status) (\s@DBClusterRole' {} a -> s {status = a} :: DBClusterRole)

instance Data.FromXML DBClusterRole where
  parseXML x =
    DBClusterRole'
      Prelude.<$> (x Data..@? "RoleArn")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable DBClusterRole where
  hashWithSalt _salt DBClusterRole' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData DBClusterRole where
  rnf DBClusterRole' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
