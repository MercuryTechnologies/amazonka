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
-- Module      : Amazonka.Kafka.Types.Tls
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.Tls where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details for client authentication using TLS.
--
-- /See:/ 'newTls' smart constructor.
data Tls = Tls'
  { -- | Specifies whether you want to enable or disable TLS authentication.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | List of ACM Certificate Authority ARNs.
    certificateAuthorityArnList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Tls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'tls_enabled' - Specifies whether you want to enable or disable TLS authentication.
--
-- 'certificateAuthorityArnList', 'tls_certificateAuthorityArnList' - List of ACM Certificate Authority ARNs.
newTls ::
  Tls
newTls =
  Tls'
    { enabled = Prelude.Nothing,
      certificateAuthorityArnList = Prelude.Nothing
    }

-- | Specifies whether you want to enable or disable TLS authentication.
tls_enabled :: Lens.Lens' Tls (Prelude.Maybe Prelude.Bool)
tls_enabled = Lens.lens (\Tls' {enabled} -> enabled) (\s@Tls' {} a -> s {enabled = a} :: Tls)

-- | List of ACM Certificate Authority ARNs.
tls_certificateAuthorityArnList :: Lens.Lens' Tls (Prelude.Maybe [Prelude.Text])
tls_certificateAuthorityArnList = Lens.lens (\Tls' {certificateAuthorityArnList} -> certificateAuthorityArnList) (\s@Tls' {} a -> s {certificateAuthorityArnList = a} :: Tls) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Tls where
  parseJSON =
    Core.withObject
      "Tls"
      ( \x ->
          Tls'
            Prelude.<$> (x Core..:? "enabled")
            Prelude.<*> ( x Core..:? "certificateAuthorityArnList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Tls where
  hashWithSalt salt' Tls' {..} =
    salt'
      `Prelude.hashWithSalt` certificateAuthorityArnList
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData Tls where
  rnf Tls' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf certificateAuthorityArnList

instance Core.ToJSON Tls where
  toJSON Tls' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("enabled" Core..=) Prelude.<$> enabled,
            ("certificateAuthorityArnList" Core..=)
              Prelude.<$> certificateAuthorityArnList
          ]
      )
