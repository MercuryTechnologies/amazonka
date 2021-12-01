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
-- Module      : Amazonka.Kafka.Types.ClientAuthentication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ClientAuthentication where

import qualified Amazonka.Core as Core
import Amazonka.Kafka.Types.Sasl
import Amazonka.Kafka.Types.Tls
import Amazonka.Kafka.Types.Unauthenticated
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Includes all client authentication information.
--
-- /See:/ 'newClientAuthentication' smart constructor.
data ClientAuthentication = ClientAuthentication'
  { -- | Details for ClientAuthentication using SASL.
    sasl :: Prelude.Maybe Sasl,
    -- | Details for ClientAuthentication using TLS.
    tls :: Prelude.Maybe Tls,
    -- | Contains information about unauthenticated traffic to the cluster.
    unauthenticated :: Prelude.Maybe Unauthenticated
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sasl', 'clientAuthentication_sasl' - Details for ClientAuthentication using SASL.
--
-- 'tls', 'clientAuthentication_tls' - Details for ClientAuthentication using TLS.
--
-- 'unauthenticated', 'clientAuthentication_unauthenticated' - Contains information about unauthenticated traffic to the cluster.
newClientAuthentication ::
  ClientAuthentication
newClientAuthentication =
  ClientAuthentication'
    { sasl = Prelude.Nothing,
      tls = Prelude.Nothing,
      unauthenticated = Prelude.Nothing
    }

-- | Details for ClientAuthentication using SASL.
clientAuthentication_sasl :: Lens.Lens' ClientAuthentication (Prelude.Maybe Sasl)
clientAuthentication_sasl = Lens.lens (\ClientAuthentication' {sasl} -> sasl) (\s@ClientAuthentication' {} a -> s {sasl = a} :: ClientAuthentication)

-- | Details for ClientAuthentication using TLS.
clientAuthentication_tls :: Lens.Lens' ClientAuthentication (Prelude.Maybe Tls)
clientAuthentication_tls = Lens.lens (\ClientAuthentication' {tls} -> tls) (\s@ClientAuthentication' {} a -> s {tls = a} :: ClientAuthentication)

-- | Contains information about unauthenticated traffic to the cluster.
clientAuthentication_unauthenticated :: Lens.Lens' ClientAuthentication (Prelude.Maybe Unauthenticated)
clientAuthentication_unauthenticated = Lens.lens (\ClientAuthentication' {unauthenticated} -> unauthenticated) (\s@ClientAuthentication' {} a -> s {unauthenticated = a} :: ClientAuthentication)

instance Core.FromJSON ClientAuthentication where
  parseJSON =
    Core.withObject
      "ClientAuthentication"
      ( \x ->
          ClientAuthentication'
            Prelude.<$> (x Core..:? "sasl")
            Prelude.<*> (x Core..:? "tls")
            Prelude.<*> (x Core..:? "unauthenticated")
      )

instance Prelude.Hashable ClientAuthentication where
  hashWithSalt salt' ClientAuthentication' {..} =
    salt' `Prelude.hashWithSalt` unauthenticated
      `Prelude.hashWithSalt` tls
      `Prelude.hashWithSalt` sasl

instance Prelude.NFData ClientAuthentication where
  rnf ClientAuthentication' {..} =
    Prelude.rnf sasl
      `Prelude.seq` Prelude.rnf unauthenticated
      `Prelude.seq` Prelude.rnf tls

instance Core.ToJSON ClientAuthentication where
  toJSON ClientAuthentication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sasl" Core..=) Prelude.<$> sasl,
            ("tls" Core..=) Prelude.<$> tls,
            ("unauthenticated" Core..=)
              Prelude.<$> unauthenticated
          ]
      )
