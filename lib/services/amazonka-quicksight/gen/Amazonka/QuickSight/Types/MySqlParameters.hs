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
-- Module      : Amazonka.QuickSight.Types.MySqlParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MySqlParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The parameters for MySQL.
--
-- /See:/ 'newMySqlParameters' smart constructor.
data MySqlParameters = MySqlParameters'
  { -- | Host.
    host :: Prelude.Text,
    -- | Port.
    port :: Prelude.Natural,
    -- | Database.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MySqlParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'mySqlParameters_host' - Host.
--
-- 'port', 'mySqlParameters_port' - Port.
--
-- 'database', 'mySqlParameters_database' - Database.
newMySqlParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  -- | 'database'
  Prelude.Text ->
  MySqlParameters
newMySqlParameters pHost_ pPort_ pDatabase_ =
  MySqlParameters'
    { host = pHost_,
      port = pPort_,
      database = pDatabase_
    }

-- | Host.
mySqlParameters_host :: Lens.Lens' MySqlParameters Prelude.Text
mySqlParameters_host = Lens.lens (\MySqlParameters' {host} -> host) (\s@MySqlParameters' {} a -> s {host = a} :: MySqlParameters)

-- | Port.
mySqlParameters_port :: Lens.Lens' MySqlParameters Prelude.Natural
mySqlParameters_port = Lens.lens (\MySqlParameters' {port} -> port) (\s@MySqlParameters' {} a -> s {port = a} :: MySqlParameters)

-- | Database.
mySqlParameters_database :: Lens.Lens' MySqlParameters Prelude.Text
mySqlParameters_database = Lens.lens (\MySqlParameters' {database} -> database) (\s@MySqlParameters' {} a -> s {database = a} :: MySqlParameters)

instance Core.FromJSON MySqlParameters where
  parseJSON =
    Core.withObject
      "MySqlParameters"
      ( \x ->
          MySqlParameters'
            Prelude.<$> (x Core..: "Host")
            Prelude.<*> (x Core..: "Port")
            Prelude.<*> (x Core..: "Database")
      )

instance Prelude.Hashable MySqlParameters where
  hashWithSalt salt' MySqlParameters' {..} =
    salt' `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` host

instance Prelude.NFData MySqlParameters where
  rnf MySqlParameters' {..} =
    Prelude.rnf host `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf port

instance Core.ToJSON MySqlParameters where
  toJSON MySqlParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Core..= host),
            Prelude.Just ("Port" Core..= port),
            Prelude.Just ("Database" Core..= database)
          ]
      )
