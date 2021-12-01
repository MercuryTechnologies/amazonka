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
-- Module      : Amazonka.QuickSight.Types.MariaDbParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MariaDbParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The parameters for MariaDB.
--
-- /See:/ 'newMariaDbParameters' smart constructor.
data MariaDbParameters = MariaDbParameters'
  { -- | Host.
    host :: Prelude.Text,
    -- | Port.
    port :: Prelude.Natural,
    -- | Database.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MariaDbParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'mariaDbParameters_host' - Host.
--
-- 'port', 'mariaDbParameters_port' - Port.
--
-- 'database', 'mariaDbParameters_database' - Database.
newMariaDbParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  -- | 'database'
  Prelude.Text ->
  MariaDbParameters
newMariaDbParameters pHost_ pPort_ pDatabase_ =
  MariaDbParameters'
    { host = pHost_,
      port = pPort_,
      database = pDatabase_
    }

-- | Host.
mariaDbParameters_host :: Lens.Lens' MariaDbParameters Prelude.Text
mariaDbParameters_host = Lens.lens (\MariaDbParameters' {host} -> host) (\s@MariaDbParameters' {} a -> s {host = a} :: MariaDbParameters)

-- | Port.
mariaDbParameters_port :: Lens.Lens' MariaDbParameters Prelude.Natural
mariaDbParameters_port = Lens.lens (\MariaDbParameters' {port} -> port) (\s@MariaDbParameters' {} a -> s {port = a} :: MariaDbParameters)

-- | Database.
mariaDbParameters_database :: Lens.Lens' MariaDbParameters Prelude.Text
mariaDbParameters_database = Lens.lens (\MariaDbParameters' {database} -> database) (\s@MariaDbParameters' {} a -> s {database = a} :: MariaDbParameters)

instance Core.FromJSON MariaDbParameters where
  parseJSON =
    Core.withObject
      "MariaDbParameters"
      ( \x ->
          MariaDbParameters'
            Prelude.<$> (x Core..: "Host")
            Prelude.<*> (x Core..: "Port")
            Prelude.<*> (x Core..: "Database")
      )

instance Prelude.Hashable MariaDbParameters where
  hashWithSalt salt' MariaDbParameters' {..} =
    salt' `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` host

instance Prelude.NFData MariaDbParameters where
  rnf MariaDbParameters' {..} =
    Prelude.rnf host `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf port

instance Core.ToJSON MariaDbParameters where
  toJSON MariaDbParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Core..= host),
            Prelude.Just ("Port" Core..= port),
            Prelude.Just ("Database" Core..= database)
          ]
      )
