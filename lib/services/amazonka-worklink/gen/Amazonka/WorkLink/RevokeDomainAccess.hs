{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkLink.RevokeDomainAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves a domain to INACTIVE status if it was in the ACTIVE status.
module Amazonka.WorkLink.RevokeDomainAccess
  ( -- * Creating a Request
    RevokeDomainAccess (..),
    newRevokeDomainAccess,

    -- * Request Lenses
    revokeDomainAccess_fleetArn,
    revokeDomainAccess_domainName,

    -- * Destructuring the Response
    RevokeDomainAccessResponse (..),
    newRevokeDomainAccessResponse,

    -- * Response Lenses
    revokeDomainAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newRevokeDomainAccess' smart constructor.
data RevokeDomainAccess = RevokeDomainAccess'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | The name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeDomainAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'revokeDomainAccess_fleetArn' - The ARN of the fleet.
--
-- 'domainName', 'revokeDomainAccess_domainName' - The name of the domain.
newRevokeDomainAccess ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  RevokeDomainAccess
newRevokeDomainAccess pFleetArn_ pDomainName_ =
  RevokeDomainAccess'
    { fleetArn = pFleetArn_,
      domainName = pDomainName_
    }

-- | The ARN of the fleet.
revokeDomainAccess_fleetArn :: Lens.Lens' RevokeDomainAccess Prelude.Text
revokeDomainAccess_fleetArn = Lens.lens (\RevokeDomainAccess' {fleetArn} -> fleetArn) (\s@RevokeDomainAccess' {} a -> s {fleetArn = a} :: RevokeDomainAccess)

-- | The name of the domain.
revokeDomainAccess_domainName :: Lens.Lens' RevokeDomainAccess Prelude.Text
revokeDomainAccess_domainName = Lens.lens (\RevokeDomainAccess' {domainName} -> domainName) (\s@RevokeDomainAccess' {} a -> s {domainName = a} :: RevokeDomainAccess)

instance Core.AWSRequest RevokeDomainAccess where
  type
    AWSResponse RevokeDomainAccess =
      RevokeDomainAccessResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RevokeDomainAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RevokeDomainAccess where
  hashWithSalt salt' RevokeDomainAccess' {..} =
    salt' `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` fleetArn

instance Prelude.NFData RevokeDomainAccess where
  rnf RevokeDomainAccess' {..} =
    Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToHeaders RevokeDomainAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RevokeDomainAccess where
  toJSON RevokeDomainAccess' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just ("DomainName" Core..= domainName)
          ]
      )

instance Core.ToPath RevokeDomainAccess where
  toPath = Prelude.const "/revokeDomainAccess"

instance Core.ToQuery RevokeDomainAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRevokeDomainAccessResponse' smart constructor.
data RevokeDomainAccessResponse = RevokeDomainAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeDomainAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'revokeDomainAccessResponse_httpStatus' - The response's http status code.
newRevokeDomainAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RevokeDomainAccessResponse
newRevokeDomainAccessResponse pHttpStatus_ =
  RevokeDomainAccessResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
revokeDomainAccessResponse_httpStatus :: Lens.Lens' RevokeDomainAccessResponse Prelude.Int
revokeDomainAccessResponse_httpStatus = Lens.lens (\RevokeDomainAccessResponse' {httpStatus} -> httpStatus) (\s@RevokeDomainAccessResponse' {} a -> s {httpStatus = a} :: RevokeDomainAccessResponse)

instance Prelude.NFData RevokeDomainAccessResponse where
  rnf RevokeDomainAccessResponse' {..} =
    Prelude.rnf httpStatus
